;;; abdicate.el --- LLM-driven Emacs agent (Responses API) -*- lexical-binding: t; -*-
;; Author: Grant Jenks <grant@example.com>
;; Version: 0.3.2
;; Package-Requires: ((emacs "28.1") (json "1.5") (cl-lib "0.6"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/YOURUSER/emacs-abdicate
;;
;; "Abdicate" - agentic Emacs
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

;; Global variable to record any evaluation errors.
(defvar abdicate--errors nil
  "A list of error messages recorded during command evaluation.")

;; ------------------------------------------------------------------------
;; Debug helper
;; ------------------------------------------------------------------------

(defun abdicate--debug-response (status)
  "Log HTTP response STATUS for debugging."
  (message "[abdicate] HTTP response status: %S" status))

;; ------------------------------------------------------------------------
;; Helper: robust JSON key lookup.
;; ------------------------------------------------------------------------
(defun abdicate--get (key alist)
  "Retrieve the value associated with KEY from ALIST.
Try both symbol and string versions of KEY."
  (or (alist-get key alist)
      (alist-get (symbol-name key) alist nil nil #'string=)))

;; ------------------------------------------------------------------------
;; Snapshot builders
;; ------------------------------------------------------------------------

(defun abdicate--window-block (win)
  "Return an XML-like <window> block for WIN."
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

(defun abdicate--snapshot ()
  "Return a pseudo-XML snapshot of Emacs state, including error information."
  (let* ((uname (or (user-login-name) "unknown"))
         (cwd  default-directory)
         (time (format-time-string "%F %T"))
         (context (format "<context>\n$USER = %s\n$CWD  = %s\n$TIME = %s\n</context>"
                          uname cwd time))
         (error-block (when abdicate--errors
                        (format "<errors>\n%s\n</errors>"
                                (string-join (reverse abdicate--errors) "\n"))))
         (windows (string-join (mapcar #'abdicate--window-block (window-list))
                                "\n\n")))
    (string-join (delq nil (list context error-block windows)) "\n\n")))

;; ------------------------------------------------------------------------
;; Prompts
;; ------------------------------------------------------------------------

(defun abdicate--system-prompt ()
  "Instruction string sent as system message.
You are \"Emacs-Agent\". You receive a USER goal and a pseudo-XML snapshot of the current Emacs windows (which may include error messages). Reply ONLY with valid JSON:
  { \"commands\": [ \"(elisp-form)\", ... ], \"continue\": true|false }
Each command is evaluated with `(eval)`. Use only built-in Emacs commands. Return `continue=false` when done."
  "You are \"Emacs-Agent\". You receive a USER goal and a pseudo-XML snapshot of the current Emacs windows (which may include error messages). Reply ONLY with valid JSON:
  { \"commands\": [ \"(elisp-form)\", ... ], \"continue\": true|false }
Each command is evaluated with `(eval)`. Use only built-in Emacs commands. Return `continue=false` when done.")

(defun abdicate--json (obj)
  "Return JSON encoding of OBJ."
  (let ((json-encoding-pretty-print nil))
    (json-encode obj)))

;; ------------------------------------------------------------------------
;; Assistant content parsing helper
;; ------------------------------------------------------------------------

(defun abdicate--parse-assistant-content (content)
  "Parse and extract string content from CONTENT.
If CONTENT is a list or vector, search for an element whose \"type\" equals \"output_text\" and return its \"text\" field.
Otherwise, if CONTENT is a string, return it directly.
Otherwise, signal an error."
  (cond
   ((stringp content)
    content)
   ((vectorp content)
    (let ((result (cl-loop for item across content
                           when (and (consp item)
                                     (string= (or (alist-get 'type item) "") "output_text"))
                           return (alist-get 'text item))))
      (if (and result (stringp result))
          result
        (error "Assistant content vector missing text field"))))
   ((listp content)
    (let ((result (cl-loop for item in content
                           when (and (consp item)
                                     (string= (or (alist-get 'type item) "") "output_text"))
                           return (alist-get 'text item))))
      (if (and result (stringp result))
          result
        (error "Assistant content list missing text field"))))
   (t
    (error "Invalid assistant content type: %s" (type-of content)))))

;; ------------------------------------------------------------------------
;; API call (Responses API, 2025) using built-in URL package instead of request
;; ------------------------------------------------------------------------

(defun abdicate--query (goal snapshot)
  "POST GOAL and SNAPSHOT; return parsed assistant JSON.
Logs the raw response for debugging. Uses the built-in `url-retrieve-synchronously`
to perform a synchronous HTTP request."
  (let* ((input `[((type . "message") (role . "system")
                   (content . ,(abdicate--system-prompt)))
                  ((type . "message") (role . "user")
                   (content . ,goal))
                  ((type . "message") (role . "user")
                   (content . ,snapshot))])
         (payload (abdicate--json `((model . ,abdicate-model)
                                     (input . ,input))))
         (url "https://api.openai.com/v1/responses"))
    (setq url-request-method "POST")
    (setq url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " abdicate-api-key))))
    (setq url-request-data (encode-coding-string payload 'utf-8))
    (let ((url-buffer (url-retrieve-synchronously url t t)))
      (unless url-buffer
        (error "Network error: no response buffer"))
      (with-current-buffer url-buffer
        (unwind-protect
            (progn
              (goto-char (point-min))
              (unless (search-forward "\n\n" nil t)
                (kill-buffer url-buffer)
                (error "Response format error: header-body separator not found"))
              (let* ((response-text (buffer-substring-no-properties (point) (point-max))))
                (message "Assistant raw response: %s" response-text)
                (let* ((data (condition-case err
                                 (json-read-from-string response-text)
                               (json-error
                                (error "Error parsing JSON response: %s" (error-message-string err)))))
                       (output (alist-get 'output data))
                       (msg (or (seq-find (lambda (it)
                                              (string= (alist-get 'type it) "message"))
                                            (reverse output))
                                (error "No assistant message in response")))
                       (raw-content (alist-get 'content msg))
                       (content-str (abdicate--parse-assistant-content raw-content)))
                  (message "Parsed assistant content: %s" content-str)
                  (condition-case err
                      (json-read-from-string content-str)
                    (json-error
                     (error "Assistant content not valid JSON: %s" content-str))))))
          (when (buffer-live-p url-buffer)
            (kill-buffer url-buffer)))))))

;; ------------------------------------------------------------------------
;; Command execution with robust error handling and retry
;; ------------------------------------------------------------------------

(defun abdicate--eval (cmd)
  "Read and evaluate CMD string.
Logs the command before evaluation. If evaluation fails, records the error
message in the snapshot and retries once.
In noninteractive mode the command is always auto-confirmed."
  (condition-case outer
      (let ((form (read cmd)))
        (message "Command to eval: %S" form)
        (if (or abdicate-auto-confirm noninteractive
                (yes-or-no-p (format "Eval %S ? " form)))
            (let ((result nil))
              (condition-case err
                  (progn
                    (setq result (eval form))
                    (message "Evaluation succeeded: %S => %S" form result))
                (error
                 (let ((err-msg (format "Error evaluating %S: %s" form (error-message-string err))))
                   (message "%s" err-msg)
                   (push err-msg abdicate--errors)
                   ;; Retry once
                   (message "Retrying command: %S" form)
                   (condition-case err2
                       (progn
                         (setq result (eval form))
                         (message "Retry succeeded: %S => %S" form result))
                     (error
                      (let ((err-msg2 (format "Retry failed for %S: %s" form (error-message-string err2))))
                        (message "%s" err-msg2)
                        (push err-msg2 abdicate--errors)
                        (setq result nil)))))))
          (progn
            (message "Skipping evaluation of: %S" form)
            nil)))
    (error (message "Unexpected error during evaluation of command: %s" (error-message-string outer))
           nil))))

;; ------------------------------------------------------------------------
;; Interactive entry point
;; ------------------------------------------------------------------------

;;;###autoload
(defun abdicate ()
  "Start an LLM-driven editing loop with robust error recording and retry on failures.
Commands that fail will have their error messages appended to the XML snapshot,
which is passed along to the assistant.
In noninteractive mode, the goal is taken from `command-line-args-left`."
  (interactive)
  (setq abdicate--errors nil)  ; Reset error log.
  (unless (stringp abdicate-api-key)
    (setq abdicate-api-key
          (if noninteractive
              (or (car command-line-args-left)
                  (error "No goal provided in noninteractive mode"))
            (read-string "OpenAI API key: " nil 'abdicate-key))))
  (let* ((goal (if noninteractive
                   (or (car command-line-args-left)
                       (error "No goal provided in noninteractive mode"))
                 (string-trim (read-string "What should the agent do? "))))
         (done nil))
    (unless (string-empty-p goal)
      (catch 'stop
        (while (not done)
          (let* ((snapshot (abdicate--snapshot))
                 (reply    (abdicate--query goal snapshot))
                 (cmds     (abdicate--get 'commands reply))
                 (cont     (or (abdicate--get 'continue reply) t)))
            (unless (or (listp cmds) (vectorp cmds))
              (error "Assistant JSON missing commands"))
            (dolist (c (if (vectorp cmds) (append cmds nil) cmds))
              (abdicate--eval c))
            (unless cont (throw 'stop t))))))))

(provide 'abdicate)

;;; abdicate.el ends here
