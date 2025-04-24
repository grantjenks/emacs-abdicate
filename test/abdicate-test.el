;;; abdicate-test.el --- Basic ERT tests for abdicate.el -*- lexical-binding: t; -*-
;;
;; Tests that core non-network functions behave as expected.

(require 'ert)
(require 'abdicate)

(ert-deftest abdicate-system-prompt-not-empty ()
  "System prompt should be a non-empty string."
  (let ((prompt (abdicate--system-prompt)))
    (should (stringp prompt))
    (should (> (length prompt) 0))))

(ert-deftest abdicate-json-encoding ()
  "abdicate--json should produce JSON containing expected keys and values."
  (let* ((obj '((foo . "bar") (num . 42)))
         (json (abdicate--json obj)))
    (should (stringp json))
    (should (string-match-p "\"foo\"" json))
    (should (string-match-p "\"bar\"" json))
    (should (string-match-p "\"num\"" json))))

(ert-deftest abdicate-snapshot-basic ()
  "Snapshot should include context tags and at least one window block."
  (let ((snap (abdicate--snapshot)))
    (should (stringp snap))
    (should (string-match-p "<context>" snap))
    (should (string-match-p "</context>" snap))
    (should (string-match-p "<window name=" snap))))