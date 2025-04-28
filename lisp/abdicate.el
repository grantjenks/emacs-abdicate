;;; abdicate.el --- LLM-driven Emacs agent (fully asynchronous) -*- lexical-binding: t; -*-
;; Author: Grant Jenks <grant@example.com>
;; Version: 0.4.6
;; Package-Requires: ((emacs "28.1") (json "1.5") (cl-lib "0.6"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/YOURUSER/emacs-abdicate

;; ------------------------------------------------------------------------
;;  CHANGELOG (0.4.6)
;;
;;  • FIX: Removed the erroneous use of ‘cl-return-from’ inside the
;;    asynchronous network primitive (`abdicate--query-async`).  A
;;    `cl-return-from` can only target an enclosing `cl-block`; a plain
;;    `defun` does not introduce such a block, so the call triggered
;;    “No catch for tag: --cl-block-abdicate--query-async--” when the
;;    early-exit path (missing API key) was taken during interactive
;;    use.  The function now short-circuits with a simple `if`/`progn`,
;;    eliminating the uncaught tag error.
;;
;; ------------------------------------------------------------------------

(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'url)
(require 'seq)

(defgroup abdicate nil "LLM-driven editing helpers." :group 'external)

(defcustom abdicate-api-key (getenv "OPENAI_API_KEY")
  "OpenAI API key.
When nil or empty, the user will be prompted to enter it on first use."
  :type 'string)

(defcustom abdicate-model "gpt-4.1"
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
;; API key prompt
;; ------------------------------------------------------------------------

(defun abdicate--ensure-api-key ()
  "Ensure `abdicate-api-key' is set, prompting the user if necessary.
The value is stored in `abdicate-api-key' and in the environment
variable OPENAI_API_KEY for this Emacs session."
  (unless (and abdicate-api-key (not (string-empty-p abdicate-api-key)))
    (let ((key (read-passwd "Enter OpenAI API key: ")))
      (if (and key (not (string-empty-p key)))
          (progn
            (setq abdicate-api-key key)
            (setenv "OPENAI_API_KEY" key)
            (message "[abdicate] API key set"))
        (user-error "[abdicate] API key is required")))))

;; ------------------------------------------------------------------------
;; Per-session state
;; ------------------------------------------------------------------------

(cl-defstruct abdicate--session
  goal           ; USER goal (string)
  errors         ; list of error strings (newest first)
  continue       ; last assistant continue flag
  turn)          ; integer turn counter

;; The active session (buffer-local).
(defvar-local abdicate--current-session nil)

;; ------------------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------------------

(defun abdicate--get (key alist)
  "Retrieve KEY from ALIST (works with both symbol and string keys)."
  (or (alist-get key alist)
      (alist-get (symbol-name key) alist nil nil #'string=)))

(defun abdicate--system-prompt ()
  "Return the system instruction string."
  "You are \"Emacs-Agent\". You receive a USER goal and a pseudo-XML snapshot of the current Emacs windows (which may include error messages). Reply ONLY with valid JSON:
  { \"commands\": [ \"(elisp-form)\", ... ], \"continue\": true|false }
Each command is evaluated with `(eval)`. Use only built-in Emacs commands. Return `continue=false` when done.")

(defun abdicate--json (obj)
  "Encode OBJ as compact JSON."
  (let ((json-encoding-pretty-print nil))
    (json-encode obj)))

;; ------------------------------------------------------------------------
;; Snapshots
;; ------------------------------------------------------------------------

(defun abdicate--window-block (win)
  "Return a <window> block describing WIN."
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
  "Return a pseudo-XML snapshot of the current state.
ERRORS is an optional list of error strings."
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
;; Assistant content parsing
;; ------------------------------------------------------------------------

(defun abdicate--parse-assistant-content (content)
  "Extract the plain string from CONTENT returned by the API."
  (cond
   ((stringp content) content)
   ((vectorp content)
    (or (cl-loop for item across content
                 when (and (consp item)
                           (string= (or (alist-get 'type item) "") "output_text"))
                 return (alist-get 'text item))
        (error "assistant content vector missing text field")))
   ((listp content)
    (or (cl-loop for item in content
                 when (and (consp item)
                           (string= (or (alist-get 'type item) "") "output_text"))
                 return (alist-get 'text item))
        (error "assistant content list missing text field")))
   (t (error "invalid assistant content type: %s" (type-of content)))))

;; ------------------------------------------------------------------------
;; Networking – asynchronous primitive
;; ------------------------------------------------------------------------

(defun abdicate--query-async (goal snapshot callback)
  "Send GOAL and SNAPSHOT to OpenAI.
CALLBACK is invoked as (FUNCALL CALLBACK &rest ARGS):
 • on success:  ARGS = (ASSISTANT-JSON)
 • on failure:  ARGS = (:error MESSAGE)

Prompts for the API key if it is not yet set."
  (abdicate--ensure-api-key)
  (if (or (null abdicate-api-key) (string-empty-p abdicate-api-key))
      (progn
        (funcall callback :error "[abdicate] API key not set")
        nil)
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
                 (let ((net-err (plist-get status :error)))
                   (if net-err
                       (funcall callback :error
                                (format "network error: %S" net-err))
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
                       (funcall callback assistant))))
               (error
                (funcall callback :error (error-message-string err))))
           (kill-buffer (current-buffer))))
       nil t))))

;; ------------------------------------------------------------------------
;; Synchronous compatibility wrapper
;; ------------------------------------------------------------------------

(defun abdicate--query (goal snapshot &optional timeout)
  "Blocking wrapper around `abdicate--query-async'.
RETURN the assistant JSON response or signal an error.
TIMEOUT (seconds) defaults to `abdicate-sync-timeout'."
  (abdicate--ensure-api-key)
  (when (or (null abdicate-api-key) (string-empty-p abdicate-api-key))
    (error "[abdicate] API key not set"))
  (let* ((timeout (or timeout abdicate-sync-timeout))
         (result  nil)
         (done    nil)
         (start   (float-time)))
    (abdicate--query-async
     goal snapshot
     (lambda (&rest reply)
       (setq result reply
             done   t)))
    (while (and (not done)
                (< (- (float-time) start) timeout))
      (accept-process-output nil 0.1))
    (unless done
      (error "abdicate--query timed out after %s s" timeout))
    (when (eq (car result) :error)
      (error "[abdicate] %s" (or (cadr result) "unknown error")))
    (car result)))

;; ------------------------------------------------------------------------
;; Command evaluation helpers
;; ------------------------------------------------------------------------

(defun abdicate--eval-command (cmd session)
  "Evaluate elisp command string CMD, recording errors in SESSION."
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

(defun abdicate--eval (cmd)
  "Default evaluative helper used inside an Abdicate session.
Takes a single argument CMD (an elisp form expressed as a string)
and delegates to `abdicate--eval-command', recording any errors in
`abdicate--current-session'."
  (when abdicate--current-session
    (abdicate--eval-command cmd abdicate--current-session)))

;; ------------------------------------------------------------------------
;; Conversation loop
;; ------------------------------------------------------------------------

(defun abdicate--continue-session (session)
  "Execute one conversation turn for SESSION."
  (let* ((abdicate--current-session session)
         (goal     (abdicate--session-goal session))
         (snapshot (abdicate--snapshot (abdicate--session-errors session))))
    (abdicate--query-async
     goal snapshot
     (lambda (&rest reply)
       (if (eq (car reply) :error)
           (message "[abdicate] %s" (cadr reply))
         (let* ((assistant (car reply))
                (cmds (abdicate--get 'commands assistant))
                (cont (abdicate--get 'continue assistant)))
           (cl-incf (abdicate--session-turn session))
           (unless (listp cmds) (setq cmds (append cmds nil)))
           (dolist (c cmds) (abdicate--eval c))
           (if (and (null (abdicate--session-errors session))
                    (not cont))
               (progn
                 (message "[abdicate] finished after %d turn%s."
                          (abdicate--session-turn session)
                          (if (= 1 (abdicate--session-turn session)) "" "s"))
                 (setq abdicate--current-session nil))
             (run-at-time 0.2 nil #'abdicate--continue-session session))))))))

;; ------------------------------------------------------------------------
;; User-visible commands
;; ------------------------------------------------------------------------

;;;###autoload
(defun abdicate (&optional goal)
  "Begin an asynchronous Abdicate session.
Interactively, read GOAL from the minibuffer if not supplied."
  (interactive)
  (when abdicate--current-session
    (user-error "An Abdicate session is already running"))
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

;;;###autoload
(defun abdicate-abort ()
  "Abort the current Abdicate session, if any."
  (interactive)
  (if (not abdicate--current-session)
      (message "[abdicate] no active session to abort.")
    (setf (abdicate--session-continue abdicate--current-session) nil)
    (setq abdicate--current-session nil)
    (message "[abdicate] session aborted.")))

(provide 'abdicate)
;;; abdicate.el ends here
