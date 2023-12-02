;;; COLORSCHEME CONFIGURATION
;; For this colorscheme, I will be using 'catppuccin' for now, although
;; I may add some sort of dynamic colorscheme configuration later
;; mayhaps something with 'pywal'?

;; Set up catppuccin theme 
;; (use-package catppuccin-theme
;;   :init (setq catppuccin-flavor 'mocha)
;;   (load-theme 'catppuccin :no-confirm))
(use-package doom-themes
  :init
  ;; Enable flashing of visual bell 
  (doom-themes-visual-bell-config)
  (load-theme 'doom-rouge t))
;; (load-theme 'modus-vivendi)
;; (use-package ewal
;;   :ensure t
;;   :init
;;   (setq ewal-use-built-in-always-p nil
;; 	ewal-use-built-in-on-failure-p nil))
