;;; devil-tests.el --- Tests for devil  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the internal devil logic.  Run these with M-x ert
;; RET devil- RET.

;;; Code:

(require 'ert)
(require 'devil)

(ert-deftest devil-invalid-key-p ()
  "Test if `devil--invalid-key-p' words as expected."
  (should (devil--invalid-key-p ""))
  (should (devil--invalid-key-p "C-x-C-f"))
  (should (devil--invalid-key-p "C-x CC-f"))
  (should (not (devil--invalid-key-p "C-x C-f")))
  (should (not (devil--invalid-key-p "C-M-x"))))

(ert-deftest devil-translate ()
  "Test if `devil-translate' works as expected."
  (should (string= (devil-translate (vconcat ",")) "C-"))
  (should (string= (devil-translate (vconcat ",x")) "C-x"))
  (should (string= (devil-translate (vconcat ",x,")) "C-x C-"))
  (should (string= (devil-translate (vconcat ",x,f")) "C-x C-f"))
  (should (string= (devil-translate (vconcat ",,")) ","))
  (should (string= (devil-translate (vconcat ",,,,")) ", ,"))
  (should (string= (devil-translate (vconcat ",mx")) "C-M-x"))
  (should (string= (devil-translate (vconcat ",mmx")) "M-x"))
  (should (string= (devil-translate (vconcat ",mmm")) "M-m")))

(provide 'devil-tests)
;;; devil-tests.el ends here
