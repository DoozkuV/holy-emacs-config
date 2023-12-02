;;; BASE THEME CONFIGURATION
;; These are just changes to emacs that don't alter the colorscheme
;; OR require any external packages/functions
;; Basically just modifying the base emacs functionality in order to give
;; it a more minimal appearance

;; Disable a lot of ugly modes and GUI features 
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq use-dialog-box nil)

;;; TAB CONFIGURATION
(setq tab-bar-show 1 ; Show tab bar only when more than 1 tab present
      tab-bar-new-button-show nil ; Disable new and cose button on tab bar
      tab-bar-close-button-show nil
      tab-bar-auto-width t) ; Static tab bar with if true

;;; FONT CONFIGURATION
(defun gp/configure-default-fonts (frame)
  "Configure font given initial non-daemon FRAME.
Intended for `after-make-frame-functions'."
  ;; Set the fixed pitch face
  (set-face-attribute 'default nil :font "monospace" :height 120) 
  (set-face-attribute 'fixed-pitch nil :font "monospace")
  ;; Remove the hook of this function after it is called once
  ;; This function will work for all future frames created
  (remove-hook 'after-make-frame-functions #'gp/configure-default-fonts))

(add-hook 'after-make-frame-functions #'gp/configure-default-fonts)
