;;; run-abdicate.el --- batch demo hitting the real API
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(require 'abdicate)

(setq abdicate-api-key (getenv "OPENAI_API_KEY"))
(setq abdicate-auto-confirm t)

(let* ((goal "replace foo with bar")
       (reply (abdicate--query goal (abdicate--snapshot))))
  (princ (format "Assistant reply:\n%S\n" reply)))
