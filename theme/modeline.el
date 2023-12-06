;; ;; Nicer modeline than the default 
;; (use-package smart-mode-line
;;   :init
;;   ;; Setup the modeline to play nice with the theme
;;   (setq sml/no-confirm-load-theme t
;; 	sml/theme 'respectful)
;;   (sml/setup))

(use-package doom-modeline
  :custom 
  ;; Display icons
  ;; This should be set explicitly in daemon mode
  (doom-modeline-icon t)
  ;; Minor modes
  ;; (doom-modeline-minor-modes t)
  ;; Word count
  (doom-modeline-enable-word-count nil)
  :hook (after-init . doom-modeline-mode)
  :config
  ;; Show the battery
  (display-battery-mode 1))
