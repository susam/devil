;; Customize user interface.
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)

;; Do not display file icon or name on title bar.
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; Dark theme colours.
(load-theme 'wombat)
(set-face-attribute 'menu nil :background "#444" :foreground "#eee")
(set-face-attribute 'default nil :background "#111" :foreground "#eee")
(set-face-attribute 'region nil :background "#354" :foreground "#eee")
(set-face-attribute 'isearch nil :background "#ff0" :foreground "#000")
(set-face-attribute 'lazy-highlight nil :background "#990" :foreground "#000")
(set-face-attribute 'mode-line nil :background "#444" :foreground "#ccc")
(set-face-attribute 'mode-line-inactive nil :background "#222" :foreground "#999")
(set-face-background 'cursor "#c96")
(set-face-foreground 'font-lock-comment-face "#fc0")

;; Dark theme attributes.
(set-face-attribute 'menu nil :inverse-video nil)
(set-face-attribute 'mode-line nil :box '(:style released-button))
(set-face-attribute 'mode-line-inactive nil :box '(:style pressed-button))

;; Packages.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package '(markdown-mode paredit rainbow-delimiters))
  (unless (package-installed-p package)
    (package-install package)))

;; Devil
(add-to-list 'load-path "~/git/devil/")
(require 'devil)
(setq devil-lighter " \U0001F608")
(setq devil-prompt "\U0001F608 %t")
(global-devil-mode)
(global-set-key (kbd "C-,") 'global-devil-mode)
