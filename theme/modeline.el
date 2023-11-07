;; Nicer modeline than the default 
(use-package smart-mode-line
  :init
  ;; Setup the modeline to play nice with the theme
  (setq sml/no-confirm-load-theme t
	sml/theme 'respectful)
  (sml/setup))
