;;; abdicate-bench.el  (skeletal)

(require 'abdicate) (require 'ert) (require 'cl-lib)

(defvar abdicate-bench-max-turns 8)
(defvar abdicate-bench-stats nil)  ; collects timing, command counts

(defmacro define-abdicate-bench (id setup test prompt)
  "Generate an ERT benchmark case."
  `(ert-deftest ,(intern (concat "abdicate-bench-" id)) ()
     (let ((default-directory (make-temp-file "bench-" t)))
       (with-temp-buffer
         ,setup
         ;; instrumentation
         (let* ((abdicate-auto-confirm t)
                (turns 0) (forms 0)
                (abdicate--eval
                 (lambda (cmd) (cl-incf forms) (eval (read cmd))))
                (abdicate--query
                 #'abdicate--query))       ; real network, not mocked
           (cl-letf (((symbol-function 'abdicate--eval) abdicate--eval)) ; advice
             (let ((start (float-time)))
               (catch 'stop
                 (while (< turns abdicate-bench-max-turns)
                   (cl-incf turns)
                   (abdicate prompt)))
               (push (list ,id turns forms (- (float-time) start))
                     abdicate-bench-stats))))
         (should ,test)))))

(define-abdicate-bench
  "add-docstring"
  (insert "def foo(x):\n    return x*2\n")
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\"\"\".*?\"\"\"" nil t)) ; docstring exists
  "Add a concise one-line docstring to the function in this buffer.")

(define-abdicate-bench
  "rename-variable"
  (insert "total = 0\nfor i in range(5):\n    total += i\nprint(total)\n")
  (save-excursion
    (goto-char (point-min))
    (not (search-forward "total" nil t)))       ; 'total' gone everywhere
  "Rename the variable 'total' to 'sum_total' everywhere.")
