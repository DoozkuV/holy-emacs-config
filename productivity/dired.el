;; Enable icons
(use-package all-the-icons
  :if (display-graphic-p))

;;; DIRED CONFIGURATION
;;; Function to automatically call ripdrag on highlighted files
;;;###autoload
(defun gp/dired-ripdrag (&optional args)
  "Call ripdrag on current file or all marked (or next ARG) files."
  (interactive (list (dired-get-marked-files nil current-prefix-arg))
	       dired-mode)
  (apply 'call-process "ripdrag" nil nil nil (mapcar 'expand-file-name args)))

;; In this config we use divrish, an enhanced version of dired with
;; ranger's functionality
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
   "l" 'dired-find-file
   "E" 'gp/dired-ripdrag)
  :custom
  ;; Sets the attributes that are shown on each file 
  (dirvish-attributes '(file-size file-time all-the-icons vc-state))
  :init (dirvish-override-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))
