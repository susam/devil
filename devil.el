;;; devil.el --- Minor mode for translating key sequences  -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2023 Susam Pal

;; Author: Susam Pal <susam@susam.net>
;; Maintainer: Susam Pal <susam@susam.net>
;; Version: 0.7.0-beta3
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
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Devil intercepts your devil key (comma by default) to let you type
;; key sequences without using modifier keys.  Devil is highly
;; configurable and it can be configured to perform other key sequence
;; translations.

;;; Code:


;;; Customization ====================================================

(defgroup devil '()
  "Minor mode for translating key sequences."
  :prefix "devil-"
  :group 'editing)

(defconst devil-version "0.6.0"
  "Devil version string.")

(defvar devil-mode-map (make-sparse-keymap)
  "Keymap for Devil mode.

By default, only `devil-key' is added to this keymap so that
Devil can be activated using it.  To support multiple activation
keys, this keymap may be modified to add multiple keys to
activate Devil.")

(defcustom devil-logging nil
  "Non-nil iff Devil should print log messages."
  :type 'boolean)

(defun devil-toggle-logging ()
  "Toggle the value of `devil-logging'."
  (interactive)
  (setq devil-logging (not devil-logging))
  (message "Devil: Logging %s" (if devil-logging "enabled" "disabled")))

(defmacro devil--log (format-string &rest args)
  "Write log message with the given FORMAT-STRING and ARGS."
  `(when devil-logging
     (message (concat "Devil: " ,format-string) ,@args)))

(defun devil--custom-devil-key (symbol value)
  "Set Devil key variable SYMBOL to the key sequence in given VALUE.

After setting SYMBOL to VALUE, clear all key bindings in
`devil-mode-map' and add a new key binding such that the key
sequence given in VALUE activates Devil."
  (set-default symbol value)
  (setcdr devil-mode-map nil)
  (define-key devil-mode-map value #'devil)
  (devil--log "Keymap updated to %s" devil-mode-map))

(defun devil-set-key (key)
  "Set `devil-key' to the given KEY and update `devil-mode-map'.

KEY is a string or vector that represents a sequence of
keystrokes, e.g., `\",\"', `(kbd \"<left>\")', etc.  This
function clears existing key bindings in `devil-mode-map' and
sets a single key binding in this keymap so that Devil can be
activated using the given KEY."
  (devil--custom-devil-key 'devil-key key))

(defcustom devil-key ","
  "The key sequence that begins Devil input.

Do not set this variable directly.  Either use the
`devil-set-key' function to set this variable or customize this
variable using Emacs customization features/functions.  Doing so
ensures that the `devil-mode-map' is updated correctly to use the
updated value of this variable."
  :type 'key-sequence
  :set #'devil--custom-devil-key)

(defun devil-key-executor (key)
  "Create a command to call `devil-execute-key' with KEY when invoked.

KEY is a string in the format returned by commands such as `C-h
k' (`describe-key').  Format control sequences supported by
`devil-format' may be used in KEY.

This is a convenience function that returns an interactive lambda
that may be used as a binding value for a special key defined in
`devil-special-keys'.  When the lambda returned by this function
is later invoked, it disables Devil keys temporarily and executes
the bindings bound to KEY.  This allows key sequences involving
the configured `devil-key' to be executed to produce their
original behaviour thus avoiding invoking Devil recursively."
  (lambda ()
    (interactive)
    (devil-execute-key key)))

(defcustom devil-special-keys
  (list (cons "%k %k" (devil-key-executor "%k"))
        (cons "%k SPC" (devil-key-executor "%k SPC"))
        (cons "%k RET" (devil-key-executor "%k RET"))
        (cons "%k <return>" (devil-key-executor "%k <return>"))
        (cons "%k h %k k" #'devil-describe-key)
        (cons "%k h %k l" #'devil-toggle-logging))
  "Special Devil keys that are executed as soon as they are typed.

The value of this variable is an alist where each key represents
a Devil key sequence.  If `key-description' of Devil key sequence
matches any key in this alist, the function or lambda in the
corresponding value is invoked.  Format control sequences
supported by `devil-format' may be used in the keys."
  :type '(alist :key-type string :value-type function))

(defcustom devil-translations
  (list (cons "%k m m" "C-M-")
        (cons "%k m %k" "M-,")
        (cons "%k m z" "M-")
        (cons "%k m" "M-")
        (cons "%k %k" "%k")
        (cons "%k z" "C-")
        (cons "%k"  "C-"))
  "Translation rules to convert Devil input to Emacs key sequence.

The value of this variable is an alist where each item represents
a translation rule that is applied on the `key-description' of
the Devil key sequence read from the user in order to obtain the
Emacs key sequence to be executed.  The translation rules are
applied in the sequence they occur in the alist.  For each rule,
if the key occurs anywhere in the Devil key sequence, it is
replaced with the corresponding value in the translation rule.
However, if a replacement leads to an invalid key sequence, then
that replacement is skipped.  Format control sequences supported
by `devil-format' may be used in the keys and values."
  :type '(alist :key-type string :value-type string))

(defcustom devil-repeatable-keys
  '(("%k /")
    ("%k d")
    ("%k k")
    ("%k m ^")
    ("%k m e")
    ("%k m b" "%k m f" "%k m a" "% k m e")
    ("%k m @" "%k m h")
    ("%k m y")
    ("%k p" "%k n" "%k b" "%k f" "%k a" "%k e")
    ("%k s")
    ("%k x [" "%k x ]")
    ("%k x ^" "%k x {" "%k x }")
    ("%k x o")
    ("%k x u"))
  "Devil mode repeatable key sequences arranged in groups.

The value of this variable is a list of lists.  Each item (each
inner list) of the top-level list represents a group of
repeatable key sequences.  Each item of each group is a
repeatable key sequence.  A repeatable key sequence may be
repeated merely by typing the last character in the key sequence.
After a repeatable key sequence has been typed, typing the last
character of any repeatable key sequence that belongs to the same
group executes that key sequence.

Note that only Devil key sequences that get translated to a
regular Emacs key sequence and result in the execution of an
Emacs command can be repeatable.  The special keys defined in
`devil-special-keys' are never repeatable.

Format control sequences supported by `devil-format' may be used
in the items.  Only key sequences that translate to a complete
Emacs key sequence according to `devil-translations' and execute
an Emacs command are made repeatable."
  :type '(repeat (repeat string)))

(defcustom devil-all-keys-repeatable nil
  "All successfully translated key sequences become repeatable if non-nil.

When this variable is set to non-nil all key sequences that
translate to a complete and defined Emacs key sequence become a
repeatable key sequence, i.e., every such key sequence can be
repeated merely by typing the last character in the key
sequence."
  :type 'boolean)

(defcustom devil-lighter " Devil"
  "String displayed on the mode line when Devil mode is enabled."
  :type 'string)

(defcustom devil-prompt "Devil: %t"
  "A format control string that determines the `devil' prompt.

Format control sequences supported by `devil-format' may be used
in the format control string."
  :type 'string)

(defcustom devil-describe-prompt "Describe Devil key: %t"
  "A format control string that determines the `devil-describe-key' prompt.

Format control sequences supported by `devil-format' may be used
in the format control string."
  :type 'string)


;;; Minor Mode Definition ============================================

;;;###autoload
(define-minor-mode devil-mode
  "Local minor mode to support Devil key sequences."
  :lighter devil-lighter
  (devil--log "Mode is %s in %s" devil-mode (buffer-name)))

;;;###autoload
(define-globalized-minor-mode
  global-devil-mode devil-mode devil--on
  (if global-devil-mode (devil--add-extra-keys) (devil--remove-extra-keys)))

(defun devil--on ()
  "Turn Devil mode on."
  (devil-mode 1))


;;; Bonus Key Bindings ===============================================

(defvar devil--saved-keys nil
  "Original key bindings saved by Devil.")

(defun devil--add-extra-keys ()
  "Add key bindings to keymaps for Isearch and universal argument."
  (devil--log "Adding extra key bindings")
  (setq devil--saved-keys (devil--original-keys-to-be-saved))
  (define-key isearch-mode-map devil-key #'devil)
  (define-key universal-argument-map (kbd "u") #'universal-argument-more))

(defun devil--remove-extra-keys ()
  "Remove Devil key bindings from Isearch and universal argument."
  (devil--log "Removing extra key bindings")
  (define-key isearch-mode-map (kbd ",")
    (devil--aget 'isearch-comma devil--saved-keys))
  (define-key universal-argument-map (kbd "u")
    (devil--aget 'universal-u devil--saved-keys)))

(defun devil--original-keys-to-be-saved ()
  "Return an alist of keys that will be modified by Devil."
  (list (cons 'isearch-comma (lookup-key isearch-mode-map devil-key))
        (cons 'universal-u (lookup-key universal-argument-map (kbd "u")))))


;;; Activation Commands ==============================================

(defun devil ()
  "Read and execute a Devil key sequence."
  (interactive)
  (devil--log "Activated with %s" (key-description (this-command-keys)))
  (let* ((result (devil--read-key devil-prompt (this-command-keys)))
         (key (devil--aget 'key result))
         (translated-key (devil--aget 'translated-key result))
         (binding (devil--aget 'binding result)))
    (devil--log "Read key: %s => %s => %s => %s"
                key (key-description key) translated-key binding)
    (if (not binding)
        (message "Devil: %s is undefined" translated-key)
      (devil--execute-command key binding)
      (when translated-key
        (devil--set-repeatable-keys (key-description key))))))

(defun devil-describe-key ()
  "Describe a Devil key sequence."
  (interactive)
  (devil--log "Activated with %s" (key-description (this-command-keys)))
  (let* ((result (devil--read-key devil-describe-prompt (vector)))
         (key (devil--aget 'key result))
         (translated-key (devil--aget 'translated-key result))
         (binding (devil--aget 'binding result)))
    (devil--log "Read key: %s => %s => %s => %s"
                key (key-description key) translated-key binding)
    (if translated-key
        (describe-key (list (cons (kbd translated-key) key)))
      ;; Create a transient keymap to describe special key sequence.
      (let* ((virtual-keymap (make-sparse-keymap))
             (exit-function (set-transient-map virtual-keymap)))
        (define-key virtual-keymap key binding)
        (describe-key key)
        (funcall exit-function)))))


;;; Command Lookup ===================================================

(defconst devil--fallbacks (list #'devil--terminal-key)
  "A list of functions that further translate a translated key.")

(defun devil--read-key (prompt key)
  "Read Devil key sequence.

Key events are read until it is determined to be a valid special
key sequence, a valid complete key sequence after translation to
Emacs key sequence, or an undefined key sequence after
translation to Emacs key sequence.

PROMPT is a format control string that defines the prompt to be
displayed while reading the key sequence.  Format control
sequences supported by `devil-format' may be used in PROMPT.

KEY is a vector that represents the key sequence read so far.
This function reads a new key from the user, appends it to KEY,
and then checks if the result is a valid key sequence or an
undefined key sequence.  If the result is a valid key sequence
for a special key command or an Emacs command, then the command
is executed.  Otherwise, this function calls itself recursively
to read yet another key from the user."
  (setq key (vconcat key (vector (read-event (devil-format prompt key)))))
  (or (devil--find-special-command key)
      (devil--find-regular-command key)
      (devil--read-key prompt key)))

(defun devil--find-special-command (key)
  "Find special command defined for KEY.

If the `key-description' of the given key sequence vector KEY is
found to be a special key in `devil-special-keys', the
corresponding special command is executed, and a non-nil result
is returned.  Otherwise nil is returned."
  (catch 'break
    (dolist (entry devil-special-keys)
      (when (string= (key-description key) (devil-format (car entry)))
        (devil--log "Found special command: %s => %s"
                    (key-description key) (cdr entry))
        (throw 'break (devil--binding-result key nil (cdr entry)))))))

(defun devil--find-regular-command (key)
  "Translate KEY and find command bound to it.

After translating the given key sequence vector KEY to an Emacs
key sequence, if the resulting key sequence turns out to be an
incomplete key, then nil is returned.  If it turns out to be a
complete key sequence, a non-nil result is returned."
  (devil--find-command key (devil--translate key) devil--fallbacks))

(defun devil--find-command (key translated-key fallbacks)
  "Find command bound to TRANSLATED-KEY translated from KEY.

FALLBACKS is a list of functions.  When FALLBACKS is non-nil and
no binding is found for the given TRANSLATED-KEY, the given
TRANSLATED-KEY is translated further by invoking the `car' of
this list.  Then this function is called recursively with the
`cdr' of this list."
  (let* ((parsed-key (ignore-errors (kbd translated-key)))
         (binding (when parsed-key (key-binding parsed-key))))
    (cond ((devil--incomplete-key-p translated-key)
           (devil--log "Ignoring incomplete key: %s" translated-key)
           nil)
          ((keymapp binding)
           (devil--log "Ignoring prefix key: %s" translated-key)
           nil)
          ((commandp binding)
           (devil--log "Found command: %s => %s" translated-key binding)
           (devil--binding-result key translated-key binding))
          (t
           (devil--log "Undefined key: %s => %s" translated-key binding)
           (let ((fallback-key (when fallbacks (funcall (car fallbacks)
                                                        translated-key))))
             (if (and fallback-key (not (string= translated-key fallback-key)))
                 (devil--find-command key fallback-key (cdr fallbacks))
               (devil--binding-result key translated-key nil)))))))

(defun devil--incomplete-key-p (translated-key)
  "Return t iff TRANSLATED-KEY is an incomplete Emacs key sequence."
  (string-match "[ACHMSs]-$" translated-key))

(defun devil--binding-result (key translated-key binding)
  "Create alist for the given KEY, TRANSLATED-KEY, and BINDING."
  (list (cons 'key key)
        (cons 'translated-key translated-key)
        (cons 'binding binding)))


;;; Key Translation ==================================================

(defun devil--translate (key)
  "Translate a given Devil key sequence vector to Emacs key sequence.

KEY is a key sequence vector that represents a Devil key
sequence.  The returned value is an Emacs key sequence string in
the format returned by commands such as `C-h k' (`describe-key')."
  (setq key (key-description key))
  (let ((result "")
        (index 0))
    ;; Scan Devil key from left to right.
    (while (< index (length key))
      (catch 'break
        ;; Try each translation at the current scan position.
        (dolist (entry devil-translations key)
          (let* ((from-key (devil-format (car entry)))
                 (to-key (devil-format (cdr entry)))
                 (in-key (substring key index))
                 (try-key))
            (when (string-prefix-p from-key in-key)
              ;; Apply matching translation at the current scan position.
              (setq try-key (devil--clean-key (concat result to-key)))
              (unless (devil--invalid-key-p try-key)
                ;; Translation succeeded.  Do not apply any more
                ;; translation at the current scan position.  Instead
                ;; move ahead to the next scan position.
                (setq result try-key)
                (setq index (+ index (length from-key)))
                (throw 'break t)))))
        ;; If no translation succeeded, increment scan position and
        ;; try applying translations at the new scan position.
        (let ((char (substring key index (1+ index))))
          (setq result (devil--clean-key (concat result char))))
        (setq index (1+ index))))
    (devil--normalize-ctrl-uppercase-chord result)))

(defun devil--terminal-key (translated-key)
  "Translate TRANSLATED-KEY to an Emacs key sequence for terminal Emacs.

The argument TRANSLATED-KEY is a string that represents an Emacs
key sequence returned by `devil--translate'.  Each keystroke in
the key sequence is looked up in `local-function-key-map'.  If a
match is found, it is replaced with its corresponding binding."
  (unless (devil--incomplete-key-p translated-key)
    (let ((result ""))
      (dolist (chunk (split-string translated-key " " t))
        (let* ((separator (if (string= result "") "" " "))
               (binding (lookup-key local-function-key-map (kbd chunk))))
          (when (and binding (not (keymapp binding)))
            (setq chunk (key-description binding)))
          (setq result (concat result separator chunk))))
      result)))

(defun devil--clean-key (translated-key)
  "Clean up TRANSLATED-KEY to properly formatted Emacs key sequence."
  (devil-regexp-replace "\\([ACHMSs]\\)- " "\\1-" translated-key))

(defun devil--normalize-ctrl-uppercase-chord (translated-key)
  "Normalize chords containing ctrl and uppercase letter in TRANSLATED-KEY."
  (devil-regexp-replace "C-\\(?:[ACHMs]-\\)*[A-Z]\\(?: \\|$\\)"
                        'devil--shifted-key translated-key))

(defun devil--shifted-key (translated-key)
  "Replace the last character in TRANSLATED-KEY with its shifted form."
  (let* ((hyphen-index (if (string-suffix-p " " translated-key) -2 -1))
         (prefix (substring translated-key 0 hyphen-index))
         (suffix (substring translated-key hyphen-index)))
    (concat prefix "S-" (downcase suffix))))

(defun devil--invalid-key-p (translated-key)
  "Return t iff TRANSLATED-KEY is an invalid Emacs key sequence."
  (catch 'break
    (dolist (chunk (split-string translated-key " "))
      (when (or (string= chunk "")
                (not (string-match-p "^\\(?:[ACHMSs]-\\)*\\([^-]*\\|<.*>\\)$" chunk))
                (string-match-p "\\([ACHMSs]-\\)[^ ]*\\1" chunk))
        (throw 'break t)))))


;;; Command Execution ================================================

(defun devil-execute-key (key)
  "Suppress Devil keys and execute the given KEY.

KEY is a string in the format returned by commands such as `C-h
k' (`describe-key').  Format control sequences supported by
`devil-format' may be used in KEY."
  (let ((keymap (cdr devil-mode-map))
        (key (devil-format key)))
    (setcdr devil-mode-map nil)
    (devil--remove-extra-keys)
    (devil--log "Disabling keymaps")
    (unwind-protect
        (devil--find-bindings-and-execute key)
      (devil--log "Enabling keymaps")
      (setcdr devil-mode-map keymap)
      (devil--add-extra-keys))))

(defun devil--find-bindings-and-execute (key)
  "Find bindings bound to the given KEY and execute them."
  (let ((accumulator ""))
    (dolist (chunk (split-string key " " t))
      (let ((separator (if (string= accumulator "") "" " ")))
        (setq accumulator (concat accumulator separator chunk)))
      (let* ((result (devil--find-command key accumulator devil--fallbacks))
             (binding (devil--aget 'binding result)))
        (cond ((not binding)
               (message "Devil: %s is undefined" accumulator)
               (setq accumulator ""))
              (t
               (devil--execute-command (kbd chunk) binding)
               (setq accumulator "")))))))

(defun devil--execute-command (key binding)
  "Execute the given BINDING bound to the given KEY."
  (let ((described-key (key-description key)))
    (devil--update-command-loop-info key binding)
    (devil--log-command-loop-info)
    (devil--log "Executing command: %s => %s" described-key binding)
    (call-interactively binding)))

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
  (devil--log "Found current-prefix-arg: %s; \
this-command: %s; last-command: %s; last-repeatable-command: %s; \
last-command-event: %s; char-before: %s"
              current-prefix-arg
              this-command
              last-command
              last-repeatable-command
              last-command-event
              (char-before)))

(defun devil--set-repeatable-keys (described-key)
  "Set transient map for repeatable keys in the same group as DESCRIBED-KEY."
  (let ((group (or (devil--find-repeatable-group described-key)
                   (when devil-all-keys-repeatable (list described-key)))))
    (when group
      (devil--log "Setting repeatable keys for %s: %S" described-key group)
      (devil--set-transient-map group))))

(defun devil--set-transient-map (repeatable-keys-group)
  "Set transient map for the keys in REPEATABLE-KEYS-GROUP."
  (let ((map (make-sparse-keymap)))
    (dolist (repeatable-key repeatable-keys-group)
      (let* ((key (vconcat (kbd (devil-format repeatable-key))))
             (translated-key (devil--translate key))
             (transient-key (vector (aref key (1- (length key)))))
             (result (devil--find-command key translated-key devil--fallbacks))
             (binding (devil--aget 'binding result)))
        (when binding
          (devil--log "Setting transient repeatable key: %s => %s"
                      (key-description transient-key) binding)
          (define-key map transient-key binding))))
    (set-transient-map map t)))

(defun devil--find-repeatable-group (described-key)
  "Find the repeatable keys group that DESCRIBED-KEY belongs to."
  (catch 'break
    (dolist (repeatable-group devil-repeatable-keys)
      (dolist (repeatable-key repeatable-group)
        (when (string= described-key (devil-format repeatable-key))
          (throw 'break repeatable-group))))))


;;; Utility Functions ================================================

(defun devil-format (format-string &optional key)
  "Format a Devil FORMAT-STRING.

KEY must be a key sequence vector.  The following format control
sequences are supported in FORMAT-STRING:

%k - Devil key.
%r - Devil key sequence read by Devil so far.
%t - Emacs key sequence translated from the Devil key sequence.
%% - The percent sign."
  (format-spec format-string (list (cons ?k (key-description devil-key))
                                   (cons ?r (key-description key))
                                   (cons ?t (devil--translate key))
                                   (cons ?% "%"))))

(defun devil-string-replace (from-string to-string in-string)
  "Replace FROM-STRING with TO-STRING in IN-STRING."
  (let ((case-fold-search nil))
    (replace-regexp-in-string (regexp-quote from-string)
                              to-string in-string t t)))

(defun devil-regexp-replace (regexp replacement in-string)
  "Replace REGEXP with REPLACEMENT in IN-STRING."
  (let ((case-fold-search nil))
    (replace-regexp-in-string regexp replacement in-string t)))

(defun devil--aget (key alist)
  "Find KEY in ALIST and return corresponding value."
  (cdr (assoc key alist)))

(provide 'devil)
;;; devil.el ends here
