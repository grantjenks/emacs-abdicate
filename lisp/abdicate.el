;;; abdicate.el --- LLM-driven Emacs agent (fully asynchronous)  -*- lexical-binding: t; -*-
;; Author: Grant Jenks <grant@example.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1") (json "1.5") (cl-lib "0.6"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/YOURUSER/emacs-abdicate
;;
;; ------------------------------------------------------------------------
;;  THIS VERSION WORKS 100 % ASYNCHRONOUSLY *AND* OFFERS
;;  A SYNCHRONOUS COMPATIBILITY WRAPPER.
;;
;;  • `abdicate--query-async`  – non-blocking network call (unchanged).
;;  • `abdicate--query`       – NEW! synchronous helper that waits
;;    for the async reply.  Batch scripts that used the old blocking
;;    helper now work again without any changes.
;;
;;  • `abdicate--snapshot`    – now accepts its ERROR list argument
;;    as OPTIONAL so existing callers with zero arguments continue to
;;    work.
;;
;; ------------------------------------------------------------------------

(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'url)
(require 'seq)

(defgroup abdicate nil "LLM-driven editing helpers." :group 'external)

(defcustom abdicate-api-key (getenv "OPENAI_API_KEY")
  "OpenAI API key."
  :type 'string)

(defcustom abdicate-model "o3"
  "Model name for /v1/responses."
  :type 'string)

(defcustom abdicate-max-bytes 15000
  "Maximum bytes of buffer text included per window."
  :type 'integer)

(defcustom abdicate-auto-confirm nil
  "If non-nil, evaluate model commands without prompting."
  :type 'boolean)

(defcustom abdicate-sync-timeout 60
  "Maximum number of seconds `abdicate--query' will wait for a reply."
  :type 'integer)

;; ------------------------------------------------------------------------
;; Per-session state
;; ------------------------------------------------------------------------

(cl-defstruct abdicate--session
  goal           ; USER goal (string)
  errors         ; list of error strings (newest first)
  continue       ; last assistant continue flag
  turn)          ; integer turn counter

;; Active session object (buffer-local so multiple frames/buffers can run)
(defvar-local abdicate--current-session nil)

;; ------------------------------------------------------------------------
;; Helper: robust JSON key lookup.
;; ------------------------------------------------------------------------

(defun abdicate--get (key alist)
  "Retrieve the value associated with KEY from ALIST.
Tries both symbol and string versions."
  (or (alist-get key alist)
      (alist-get (symbol-name key) alist nil nil #'string=)))

;; ------------------------------------------------------------------------
;; Snapshot builders
;; ------------------------------------------------------------------------

(defun abdicate--window-block (win)
  "Return an XML-ish <window> block for WIN."
  (with-current-buffer (window-buffer win)
    (let* ((buf  (buffer-name))
           (mode (symbol-name major-mode))
           (size (buffer-size))
           (max  abdicate-max-bytes)
           (trunc (> size max))
           (body (if trunc
                     (concat (buffer-substring-no-properties 1 max)
                             (format "\n...[truncated %d bytes]..." (- size max)))
                   (buffer-substring-no-properties 1 (1+ size)))))
      (format "<window name=\"%s\" mode=\"%s\" truncated=\"%s\">\n%s\n</window>"
              buf mode (if trunc "yes" "no") body))))

(defun abdicate--snapshot (&optional errors)
  "Return a pseudo-XML snapshot of Emacs state.
ERRORS is a list of error strings (may be nil).  It is optional so
existing third-party callers that passed no arguments continue to
work."
  (let* ((uname (or (user-login-name) "unknown"))
         (cwd   default-directory)
         (time  (format-time-string "%F %T"))
         (context (format "<context>\n$USER = %s\n$CWD  = %s\n$TIME = %s\n</context>"
                          uname cwd time))
         (error-block (when errors
                        (format "<errors>\n%s\n</errors>"
                                (string-join (reverse errors) "\n"))))
         (windows (string-join
                   (mapcar #'abdicate--window-block (window-list))
                   "\n\n")))
    (string-join (delq nil (list context error-block windows)) "\n\n")))

;; ------------------------------------------------------------------------
;; Prompts & JSON helpers
;; ------------------------------------------------------------------------

(defun abdicate--system-prompt ()
  "Return the system instruction string."
  "You are \"Emacs-Agent\". You receive a USER goal and a pseudo-XML snapshot of the current Emacs windows (which may include error messages). Reply ONLY with valid JSON:\n  { \"commands\": [ \"(elisp-form)\", ... ], \"continue\": true|false }\nEach command is evaluated with `(eval)`. Use only built-in Emacs commands. Return `continue=false` when done.")

(defun abdicate--json (obj)
  "Encode OBJ as compact JSON."
  (let ((json-encoding-pretty-print nil))
    (json-encode obj)))

;; ------------------------------------------------------------------------
;; Assistant content parsing helper
;; ------------------------------------------------------------------------

(defun abdicate--parse-assistant-content (content)
  "Extract plain string from CONTENT returned by the Responses API."
  (cond
   ((stringp content) content)
   ((vectorp content)
    (let ((res
           (cl-loop for item across content
                    when (and (consp item)
                              (string= (or (alist-get 'type item) "") "output_text"))
                    return (alist-get 'text item))))
      (if res res (error "assistant content vector missing text field"))))
   ((listp content)
    (let ((res
           (cl-loop for item in content
                    when (and (consp item)
                              (string= (or (alist-get 'type item) "") "output_text"))
                    return (alist-get 'text item))))
      (if res res (error "assistant content list missing text field"))))
   (t (error "invalid assistant content type: %s" (type-of content)))))

;; ------------------------------------------------------------------------
;; Networking – asynchronous primitive
;; ------------------------------------------------------------------------

(defun abdicate--query-async (goal snapshot callback)
  "Send GOAL and SNAPSHOT to OpenAI and call CALLBACK with parsed JSON.
CALLBACK is invoked as (funcall CALLBACK parsed-json) on success, or
(funcall CALLBACK :error message) on failure."
  (let* ((input   `[((type . "message") (role . "system") (content . ,(abdicate--system-prompt)))
                    ((type . "message") (role . "user")   (content . ,goal))
                    ((type . "message") (role . "user")   (content . ,snapshot))])
         (payload (abdicate--json `((model . ,abdicate-model) (input . ,input))))
         (url     "https://api.openai.com/v1/responses")
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type"  . "application/json")
                                      ("Authorization" . ,(concat "Bearer " abdicate-api-key))))
         (url-request-data (encode-coding-string payload 'utf-8)))
    (url-retrieve
     url
     (lambda (status)
       (unwind-protect
           (condition-case err
               (progn
                 ;; Transport-level errors come in via the :error property.
                 (when (plist-get status :error)
                   (cl-return-from abdicate--query-async
                     (funcall callback :error
                              (format "network error: %S"
                                      (plist-get status :error)))))
                 ;; Skip HTTP headers.
                 (goto-char (point-min))
                 (re-search-forward "\n\n" nil :noerror)
                 (let* ((body (buffer-substring-no-properties (point) (point-max)))
                        (data (json-read-from-string body))
                        (output (alist-get 'output data))
                        (msg (or (seq-find (lambda (it)
                                             (string= (alist-get 'type it) "message"))
                                           (reverse output))
                                 (error "no assistant message in output")))
                        (raw (alist-get 'content msg))
                        (txt (abdicate--parse-assistant-content raw))
                        (assistant (json-read-from-string txt)))
                   (funcall callback assistant)))
             (error
              (funcall callback :error (error-message-string err))))
         (kill-buffer (current-buffer))))
     nil t)))

;; ------------------------------------------------------------------------
;; NEW synchronous compatibility wrapper
;; ------------------------------------------------------------------------

(defun abdicate--query (goal snapshot &optional timeout)
  "Blocking helper built on top of `abdicate--query-async'.
Returns the assistant JSON response.  Signals an error on failure.
TIMEOUT (seconds) defaults to `abdicate-sync-timeout'.

This exists solely for non-interactive/batch scripts that relied on
the original synchronous implementation."
  (let* ((timeout (or timeout abdicate-sync-timeout))
         (result  nil)
         (done    nil)
         (start   (float-time)))
    (abdicate--query-async
     goal snapshot
     (lambda (reply)
       (setq result reply
             done   t)))
    (while (and (not done)
                (< (- (float-time) start) timeout))
      ;; Wait for network data; 0.1s keeps CPU usage negligible.
      (accept-process-output nil 0.1))
    (unless done
      (error "abdicate--query timed out after %s s" timeout))
    (when (eq result :error)
      (error "[abdicate] %s" (or (cadr result) "unknown error")))
    result))

;; ------------------------------------------------------------------------
;; Command execution (unchanged but records into session)
;; ------------------------------------------------------------------------

(defun abdicate--eval-command (cmd session)
  "Evaluate single elisp CMD string, recording any error into SESSION."
  (let ((errors (abdicate--session-errors session)))
    (condition-case outer
        (let ((form
               (condition-case err
                   (read cmd)
                 (error
                  (let ((msg (format "Error reading %S: %s"
                                     cmd (error-message-string err))))
                    (message "%s" msg)
                    (push msg errors)
                    nil)))))
          (when form
            (message "ABDICATE EVAL: %S" form)
            (if (or abdicate-auto-confirm noninteractive
                    (yes-or-no-p (format "Eval %S ? " form)))
                (condition-case err
                    (eval form)
                  (error
                   (let ((msg (format "Error evaluating %S: %s"
                                      form (error-message-string err))))
                     (message "%s" msg)
                     (push msg errors))))
              (message "Skipped %S" form))))
      (error
       (let ((msg (format "Unexpected eval error: %S"
                          (error-message-string outer))))
         (message "%s" msg)
         (push msg errors))))
    (setf (abdicate--session-errors session) errors)))

;; ------------------------------------------------------------------------
;; Conversation loop – trampoline style
;; ------------------------------------------------------------------------

(defun abdicate--continue-session (session)
  "Carry out one conversation turn of SESSION, then return immediately.
Further turns happen automatically via asynchronous callbacks."
  (let* ((goal     (abdicate--session-goal session))
         (snapshot (abdicate--snapshot (abdicate--session-errors session))))
    (abdicate--query-async
     goal snapshot
     (lambda (reply)
       (if (eq reply :error)
           (message "[abdicate] %s" reply) ; already logged
         (cl-incf (abdicate--session-turn session))
         (let* ((cmds (abdicate--get 'commands reply))
                (cont (abdicate--get 'continue reply)))
           ;; Tolerate single string, vector, or list
           (unless (listp cmds) (setq cmds (append cmds nil)))
           (dolist (c cmds) (abdicate--eval-command c session))
           (if (and (null (abdicate--session-errors session))
                    (not cont))
               (progn
                 (message "[abdicate] finished after %d turn%s."
                          (abdicate--session-turn session)
                          (if (= 1 (abdicate--session-turn session)) "" "s"))
                 (setq abdicate--current-session nil))
             ;; schedule next turn soon (yield to redisplay)
             (run-at-time 0.2 nil #'abdicate--continue-session session))))))))

;; ------------------------------------------------------------------------
;; Interactive entry point
;; ------------------------------------------------------------------------

;;;###autoload
(defun abdicate (&optional goal)
  "Start an asynchronous LLM-driven editing session.
If GOAL is nil interactively, read it from the minibuffer."
  (interactive)
  (when abdicate--current-session
    (user-error "An abdicate session is already running"))
  (let ((g (or goal
               (if noninteractive
                   (or (car command-line-args-left)
                       (error "No goal given"))
                 (string-trim (read-string "Prompt: "))))))
    (unless (string-empty-p g)
      (setq abdicate--current-session
            (make-abdicate--session :goal g :errors nil :continue t :turn 0))
      (message "[abdicate] starting async session…")
      (abdicate--continue-session abdicate--current-session))))

(provide 'abdicate)
;;; abdicate.el ends here
