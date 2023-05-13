;;; devil.el --- Minor mode for translating key sequences  -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2023 Susam Pal

;; Author: Susam Pal <susam@susam.net>
;; Maintainer: Susam Pal <susam@susam.net>
;; Version: 0.3.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, abbrev
;; URL: https://github.com/susam/devil

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Devil intercepts your devil key (comma by default) to let you type
;; key sequences without using modifier keys.  Devil is highly
;; configurable and it can be configured to perform other key sequence
;; translations.

;;; Code:

(defgroup devil '()
  "Minor mode for translating key sequences."
  :prefix "devil-"
  :group 'editing)

(defcustom devil-key ","
  "The key sequence that begins Devil input.

The key sequence must be specified in the format returned by `C-h
k' (`describe-key').  This variable should be set before enabling
Devil mode for it to take effect."
  :type 'key-sequence)

(defcustom devil-lighter " Devil"
  "String displayed on the mode line when Devil mode is enabled."
  :type 'string)

(defvar devil-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map devil-key #'devil)
    map)
  "Keymap to wake up Devil when `devil-key' is typed.

By default, only `devil-key' is added to this keymap so that
Devil can be activated using it.  To support multiple activation
keys, this variable may be modified to a new keymap that defines
multiple different keys to activate Devil.  This variable should
be modified before loading Devil for it to take effect.")

;;;###autoload
(define-minor-mode devil-mode
  "Local minor mode to support Devil key sequences."
  :lighter devil-lighter
  (devil--log "Mode is %s in %s" devil-mode (buffer-name)))

;;;###autoload
(define-globalized-minor-mode
  global-devil-mode devil-mode devil--on
  (if global-devil-mode (devil-add-extra-keys) (devil-remove-extra-keys)))

(defun devil--on ()
  "Turn Devil mode on."
  (devil-mode 1))

(defcustom devil-logging nil
  "Non-nil iff Devil should print log messages."
  :type 'boolean)

(defvar devil-special-keys
  (list (cons "%k %k" (lambda () (interactive) (devil-run-key "%k")))
        (cons "%k SPC" (lambda () (interactive) (devil-run-key "%k SPC")))
        (cons "%k RET" (lambda () (interactive) (devil-run-key "%k RET"))))
  "Special Devil keys that are executed as soon as they are typed.

The value of this variable is an alist where each key represents
a Devil key sequence.  If a Devil key sequence matches any key in
this alist, the function or lambda in the corresponding value is
invoked.  The format control specifier `%k' may be used to
represent `devil-key' in the keys.")

(defcustom devil-translations
  (list (cons "%k z" "C-")
        (cons "%k %k" "%k")
        (cons "%k m m" "M-")
        (cons "%k"  "C-")
        (cons "m" "M-"))
  "Translation rules to convert Devil input to Emacs key sequence.

The value of this variable is an alist where each item represents
a translation rule that is applied on the Devil key sequence read
from the user to obtain the Emacs key sequence to be executed.
The translation rules are applied in the sequence they occur in
the alist.  For each rule, if the key occurs anywhere in the Devil
key sequence, it is replaced with the corresponding value in the
translation rule.  The format control specifier `%k' may be used
to represent `devil-key' in the keys."
  :type '(alist :key-type string :value-type string))

(defcustom devil-repeatable-keys
  (list "%k p"
        "%k n"
        "%k f"
        "%k b"
        "%k k"
        "%k /"
        "%k m m f"
        "%k m m b"
        "%k m m y"
        "%k x o")
  "Devil mode repeatable key sequences.

The value of this variable is a list where each item represents a
key sequence that may be repeated merely by typing the last
character in the key sequence.  The format control specified `%k'
may be used to represent `devil-key' in the keys.  Only key
sequences that translate to a complete Emacs key sequence
according to `devil-translations' and execute an Emacs command
are made repeatable.  Key sequences that belong to
`devil-special-keys' are never made repeatable.  Note that this
variable is ignored if `devil-all-keys-repeatable' is set to t."
  :type '(repeat string))

(defcustom devil-all-keys-repeatable nil
  "All successfully translated key sequences become repeatable iff t.

When this variable is set to t all key sequences that translate
to a complete and defined Emacs key sequence become a repeatable
key sequence, i.e., it can be repeated merely by typing the last
character in the key sequence.  Note that key sequences that
belong to `devil-special-keys' are never made repeatable.  Also,
note that when this variable is set to t, the variable
`devil-repeatable-keys' is ignored.  However when this variable
is set to nil, the variable `devil-repeatable-keys' is used to
determine whether a key sequence is repeatable or not."
  :type 'boolean)

(defun devil-run-key (key)
  "Execute the given key sequence KEY.

KEY must be in the format returned by `C-h k` (`describe-key').
If the format control specifier `%k' occurs in KEY, for each such
occurrence `devil-key' is inserted into the buffer."
  (dolist (key (split-string key))
    (if (string= key "%k") (insert devil-key) (execute-kbd-macro (kbd key)))))

(defvar devil--saved-keys nil
  "Original key bindings saved by Devil.")

(defun devil-add-extra-keys ()
  "Add key bindings to keymaps for Isearch and universal argument."
  (devil--log "Adding extra key bindings")
  (setq devil--saved-keys (devil--original-keys-to-be-saved))
  (define-key isearch-mode-map (kbd devil-key) #'devil)
  (define-key universal-argument-map (kbd "u") #'universal-argument-more))

(defun devil-remove-extra-keys ()
  "Remove Devil key bindings from Isearch and universal argument."
  (devil--log "Removing extra keybindings")
  (define-key isearch-mode-map (kbd ",")
    (cdr (assoc 'isearch-comma devil--saved-keys)))
  (define-key universal-argument-map (kbd "u")
    (cdr (assoc 'universal-u devil--saved-keys))))

(defun devil--original-keys-to-be-saved ()
  "Return an alist of keys that will be modified by Devil."
  (list (cons 'isearch-comma (lookup-key isearch-mode-map (kbd devil-key)))
        (cons 'universal-u (lookup-key universal-argument-map (kbd "u")))))

(defun devil ()
  "Wake up Devil to read and translate Devil key sequences."
  (interactive)
  (devil--log "Devil waking up")
  (devil--read-key (this-command-keys)))

(defun devil--read-key (key)
  "Read Devil key sequences.

Key sequences are read until it is determined to be a valid Devil
mode special key sequence, a valid complete key sequence after
translation to Emacs key sequence, or an undefined key sequence
after translation to Emacs key sequence.

The argument KEY is a vector that represents the key sequence
read so far.  This function reads a new key from the user, appends
it to KEY, and then checks if the result is a valid key sequence
or an undefined key sequence.  If the result is a valid key
sequence for a special key command or an Emacs command, then the
command is executed.  Otherwise, this function calls itself
recursively to read yet another key from the user."
  (setq key (vconcat key (vector (read-key (devil--make-prompt key)))))
  (unless (devil--run-command key)
    (devil--read-key key)))

(defcustom devil-prompt "Devil: %t"
  "A format control string that determines the Devil prompt.

The following format control sequences are supported:

%k - Devil key sequence read by Devil so far.
%t - Emacs key sequence translated from Devil key sequence read so far.
%% - The percent sign."
  :type 'string)

(defun devil--make-prompt (key)
  "Create Devil prompt based on the given KEY."
  ;; If you are interested in adding Compat as a dependency, you can
  ;; make use of `format-spec' without raining the minimum version.
  (let ((result devil-prompt)
        (controls (list (cons "%k" (key-description key))
                        (cons "%t" (devil-translate key))
                        (cons "%%" "%"))))
    (dolist (control controls result)
      (setq result (replace-regexp-in-string (car control)
                                             (cdr control) result)))))

(defun devil--run-command (key)
  "Try running the command bound to the key sequence in KEY.

KEY is a vector that represents a sequence of keystrokes.  If KEY
is found to be a special key in `devil-special-keys', the
corresponding special command is executed immediately and t is
returned.

Otherwise, it is translated to an Emacs key sequence using
`devil-translations'.  If the resulting Emacs key sequence is
found to be a complete key sequence, the command it is bound to
is executed interactively and t is returned.  If it is found to be
an undefined key sequence, then t is returned.  If the resulting
Emacs key sequence is found to be an incomplete key sequence,
then nil is returned."
  (devil--log "Trying to execute key: %s" (key-description key))
  (or (devil--run-special-command key)
      (devil--run-regular-command key)))

(defun devil--run-special-command (key)
  "Run Devil mode special command defined for the Devil key sequence KEY.

If the given key sequence KEY is found to be a special key in
`devil-special-keys', the corresponding special command is
executed, and t is returned.  Otherwise nil is returned."
  (catch 'break
    (dolist (entry devil-special-keys)
      (when (string= (key-description key) (devil-format (car entry)))
        (devil--log "Running special command: %s => %s"
                    (key-description key) (cdr entry))
        (funcall (cdr entry))
        (throw 'break t)))))

(defun devil--run-regular-command (key)
  "Translate KEY and run command bound to it.

After translating KEY to an Emacs key sequence, if the resulting
key sequence turns out to be an incomplete key, then nil is
returned.  If it turns out to be a complete key sequence, the
corresponding Emacs command is executed, and t is returned.  If it
turns out to be an undefined key sequence, t is returned.  The
return value t indicates to the caller that no more Devil key
sequences should be read from the user."
  (let* ((described-key (key-description key))
         (translated-key (devil-translate key))
         (parsed-key (condition-case nil (kbd translated-key) (error nil)))
         (binding (when parsed-key (key-binding parsed-key))))
    (cond ((string-match "[ACHMSs]-$" translated-key)
           (devil--log "Ignoring incomplete key: %s => %s"
                       described-key translated-key)
           nil)
          ((keymapp binding)
           (devil--log "Ignoring prefix key: %s => %s => %s"
                       described-key translated-key binding)
           nil)
          ((commandp binding)
           (devil--update-command-loop-info key binding)
           (devil--log-command-loop-info)
           (devil--log "Executing key: %s => %s => %s"
                       described-key translated-key binding)
           (call-interactively binding)
           (when (or devil-all-keys-repeatable
                     (devil--repeatable-key-p described-key))
             (devil--set-transient-map (substring described-key -1) binding))
           t)
          (t
           (devil--log "Undefined key: %s => %s" described-key translated-key)
           (message "Devil: %s is undefined" translated-key)
           t))))

(defun devil-translate (key)
  "Translate a given Devil KEY to Emacs key sequence.

The argument KEY is a vector that represents the key sequence
read so far."
  (setq key (key-description key))
  (let ((result "")
        (index 0))
    (while (< index (length key))
      (catch 'break
        ;; Try translating the current position in Devil key to Emacs key.
        (dolist (entry devil-translations key)
          (let* ((from-key (devil-format (car entry)))
                 (to-key (devil-format (cdr entry)))
                 (in-key (substring key index))
                 (try-key))
            (when (string-prefix-p from-key in-key)
              (setq try-key (devil--clean-key (concat result to-key)))
              (unless (devil--invalid-key-p try-key)
                (setq result try-key)
                (setq index (+ index (length from-key)))
                (throw 'break t)))))
        ;; If no translation succeeded, advance current position.
        (let ((char (substring key index (1+ index))))
          (setq result (devil--clean-key (concat result char))))
        (setq index (1+ index))))
    result))

(defun devil--update-command-loop-info (key binding)
  "Update variables that maintain command loop information.

The given KEY and BINDING is used to update variables that
maintain command loop information.  This allows the commands that
depend on them behave as if they were being invoked directly with
the original Emacs key sequence."
  ;;
  ;; Set `last-command-event' so that `digit-argument' can determine
  ;; the correct digit for key sequences like , 5 (C-5).  See M-x
  ;; find-function RET digit-argument RET for details.
  (setq last-command-event (aref key (- (length key) 1)))
  ;;
  ;; Set `this-command' to make several commands like , z SPC , z SPC
  ;; (C-SPC C-SPC) and , p (C-p) work correctly.  Emacs copies
  ;; `this-command' to `last-command'.  Both variables are used by
  ;; `set-mark-command' to decide whether to activate/deactivate the
  ;; current mark.  The first variable is used by vertical motion
  ;; commands to keep the cursor at the `temporary-goal-column'.  There
  ;; may be other commands too that depend on this variable.
  (setq this-command binding)
  ;;
  ;; Set `real-this-command' to make , x z (C-x z) work correctly.
  ;; Emacs copies it to `last-repeatable-command' which is then used
  ;; by repeat.  See the following for more details:
  ;;
  ;;   - M-x find-function RET repeat RET
  ;;   - C-h v last-repeatable-command RET
  ;;   - grep kset_last_repeatable_command src/keyboard.c
  (setq real-this-command binding))

(defun devil--log-command-loop-info ()
  "Log command loop information for debugging purpose."
  (devil--log
   (format "Found current-prefix-arg: %s; \
this-command: %s; last-command: %s; last-repeatable-command: %s"
	   current-prefix-arg
	   this-command
	   last-command
	   last-repeatable-command)))

(defun devil--repeatable-key-p (described-key)
  "Return t iff DESCRIBED-KEY belongs to `devil-repeatable-keys'."
  (catch 'break
    (dolist (repeatable-key devil-repeatable-keys)
      (when (string= described-key (devil-format repeatable-key))
        (throw 'break t)))))

(defun devil--set-transient-map (key binding)
  "Set transient map to run BINDING with KEY."
  (devil--log "Setting transient map: %s => %s" key binding)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd key) binding)
    (set-transient-map map t)))

(defun devil--clean-key (translated-key)
  "Clean up TRANSLATED-KEY to properly formatted Emacs key sequence."
  (replace-regexp-in-string "\\([ACHMSs]\\)- " "\\1-" translated-key))

(defun devil--invalid-key-p (translated-key)
  "Return t iff TRANSLATED-KEY is an invalid Emacs key sequence."
  (catch 'break
    (dolist (chunk (split-string translated-key " "))
      (when (or (string= chunk "")
                (not (string-match-p "^\\(?:[ACHMSs]-\\)*[^ ]*$" chunk))
                (string-match-p "\\([ACHMSs]-\\)[^ ]*\\1" chunk))
        (throw 'break t)))))

(defun devil-format (string)
  "Replace %k in STRING with `devil-key'."
  (replace-regexp-in-string "%k" devil-key string))

(defun devil--log (format-string &rest args)
  "Write log message with the given FORMAT-STRING and ARGS."
  (when devil-logging
    (apply #'message (concat "Devil: " format-string) args)))

(provide 'devil)
;;; devil.el ends here
