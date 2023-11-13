(use-package all-the-icons
  :if (display-graphic-p))

;;; DIRED CONFIGURATION
;; In this config we use divrish, an enhanced
;; version of dired with ranger's functionality
(use-package dirvish
  :general
  (gp/leader-keys
    "j" '(dirvish-dwim :which-key "Dired Jump")) 
  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   "H" 'dired-hide-dotfiles-mode ; See dired-hide-dotfiles
   ;; "z" 'zoxide-travel  
   "q" 'dirvish-quit
   "h" 'dired-up-directory
   "l" 'dired-find-file)
  :custom
  ;; Sets the attributes that are shown on each file 
  (dirvish-attributes '(file-size file-time all-the-icons vc-state))
  :init (dirvish-override-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))
