;;; DIRED CONFIGURATION
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
	      ("H" . 'dired-hide-dotfiles-mode)
	      ("b" . 'dired-up-directory)))
