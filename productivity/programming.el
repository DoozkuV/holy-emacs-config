;;; PROGRAMMING
;; Fish mode configuration
(use-package fish-mode
  :hook (fish-mode . (lambda ()
		       (add-hook 'before-save-hook 'fish_indent-before-save))))

;; Yuck-mode for editing eww config files
(use-package yuck-mode
  :mode ("\\.yuck\\'" . yuck-mode))
