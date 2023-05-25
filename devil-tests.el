;;; devil-tests.el --- Tests for devil  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the internal devil logic.  Run these with M-x ert
;; RET devil- RET.

;;; Code:

(require 'ert)
(require 'devil)

(ert-deftest devil-string-replace ()
  "Test if `devil-string-replace' works as expected."
  (should (string= (devil-string-replace "" "" "") ""))
  (should (string= (devil-string-replace "" "foo" "") ""))
  (should (string= (devil-string-replace "foo" "foo" "foo") "foo"))
  (should (string= (devil-string-replace "foo" "bar" "") ""))
  (should (string= (devil-string-replace "foo" "bar" "foo") "bar"))
  (should (string= (devil-string-replace "foo" "bar" "Foo") "Foo"))
  (should (string= (devil-string-replace "foo" "bar" "FOO") "FOO"))
  (should (string= (devil-string-replace "f.." "bar" "foo f..") "foo bar"))
  (should (string= (devil-string-replace "f.." "<\\&>" "foo f..") "foo <\\&>")))

(ert-deftest devil-regexp-replace ()
  "Test if `devil-string-replace' works as expected."
  (should (string= (devil-regexp-replace "" "" "") ""))
  (should (string= (devil-regexp-replace "" "foo" "") ""))
  (should (string= (devil-regexp-replace "foo" "foo" "foo") "foo"))
  (should (string= (devil-regexp-replace "foo" "bar" "") ""))
  (should (string= (devil-regexp-replace "foo" "bar" "foo") "bar"))
  (should (string= (devil-regexp-replace "foo" "bar" "Foo") "Foo"))
  (should (string= (devil-regexp-replace "foo" "bar" "FOO") "FOO"))
  (should (string= (devil-regexp-replace "f.." "bar" "foo f..") "bar bar"))
  (should (string= (devil-regexp-replace "f.." "<\\&>" "foo f..") "<foo> <f..>")))

(ert-deftest devil-shifted-key ()
  "Test if `devil--shifted-key' works as expected."
  (should (string= (devil--shifted-key "A") "S-a"))
  (should (string= (devil--shifted-key "C-A") "C-S-a"))
  (should (string= (devil--shifted-key "C-M-A") "C-M-S-a"))
  (should (string= (devil--shifted-key "A ") "S-a "))
  (should (string= (devil--shifted-key "C-A ") "C-S-a "))
  (should (string= (devil--shifted-key "C-M-A ") "C-M-S-a ")))

(ert-deftest devil-invalid-key-p ()
  "Test if `devil--invalid-key-p' works as expected."
  (should (devil--invalid-key-p ""))
  (should (devil--invalid-key-p "C-x-C-f"))
  (should (devil--invalid-key-p "C-x CC-f"))
  (should (not (devil--invalid-key-p "C-x C-f")))
  (should (not (devil--invalid-key-p "C-M-x"))))

(ert-deftest devil-translate ()
  "Test if `devil-translate' works as expected."
  ;; Trivial translations.
  (should (string= (devil-translate (vconcat "a")) "a"))
  (should (string= (devil-translate (vconcat "A")) "A"))
  ;; Translations involving the C- modifier.
  (should (string= (devil-translate (vconcat ",")) "C-"))
  (should (string= (devil-translate (vconcat ",x")) "C-x"))
  (should (string= (devil-translate (vconcat ",x,")) "C-x C-"))
  (should (string= (devil-translate (vconcat ",x,f")) "C-x C-f"))
  ;; Escape hatch to type commas.
  (should (string= (devil-translate (vconcat ",,")) ","))
  (should (string= (devil-translate (vconcat ",,,,")) ", ,"))
  ;; Translations involving M- modifier.
  (should (string= (devil-translate (vconcat ",mx")) "C-M-x"))
  (should (string= (devil-translate (vconcat ",mmx")) "M-x"))
  (should (string= (devil-translate (vconcat ",mmm")) "M-m"))
  ;; Translations involing C- and uppercase letter.
  (should (string= (devil-translate (vconcat ",a")) "C-a"))
  (should (string= (devil-translate (vconcat ",A")) "C-S-a"))
  (should (string= (devil-translate (vconcat ",mA")) "C-M-S-a"))
  (should (string= (devil-translate (vconcat ",mmA")) "M-A"))
  (should (string= (devil-translate (vconcat ",A,mA,a")) "C-S-a C-M-S-a C-a"))
  (should (string= (devil-translate (vconcat ",AmA,mmA,a")) "C-S-a M-A M-A C-a"))
  ;; Translations involving C- and RET.
  (should (string= (devil-translate (vconcat ",\r")) "C-RET"))
  (should (string= (devil-translate (vconcat ",m\r")) "C-M-RET"))
  (should (string= (devil-translate (vconcat ",mm\r")) "M-RET"))
  (should (string= (devil-translate (vconcat ",\r,R,mm\r")) "C-RET C-S-r M-RET"))
  ;; Translations provided in the manual as examples.
  (should (string= (devil-translate (vconcat ",s")) "C-s"))
  (should (string= (devil-translate (vconcat ",ms")) "C-M-s"))
  (should (string= (devil-translate (vconcat ",z ")) "C-SPC"))
  (should (string= (devil-translate (vconcat ",zz")) "C-z"))
  (should (string= (devil-translate (vconcat ",z,")) "C-,"))
  (should (string= (devil-translate (vconcat ",cmm")) "C-c m"))
  (should (string= (devil-translate (vconcat ",cmzm")) "C-c M-m"))
  (should (string= (devil-translate (vconcat ",mzm")) "C-M-m"))
  (should (string= (devil-translate (vconcat ",mmx")) "M-x"))
  (should (string= (devil-translate (vconcat ",c,,")) "C-c ,")))

(provide 'devil-tests)
;;; devil-tests.el ends here
