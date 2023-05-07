(add-to-list 'load-path "~/git/devil/")
(require 'devil)
(add-hook 'text-mode-hook 'devil-mode)
(global-set-key (kbd "C-,") 'devil-mode)
