;;; BASIC INITALIZATION
(setq user-mail-address "georgenpadron@gmail.com")
(global-auto-revert-mode 1) ; Revert buffers when underlying file changes
(setq global-auto-revert-non-file-buffers t) ; Revert dired and more

;;; USE-PACKAGE/PACKAGE MANAGER SETUP
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;;; VERTICO COMPLETION
(use-package vertico
  :diminish
  :bind (:map vertico-map
	      ("C-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))
;; Save history of vertico between emacs session
(use-package savehist
  :init
  (savehist-mode))
;; Extra info in completion buffers
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Orderless fuzzy finding
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;;; CORFU COMPLETION
(use-package corfu
  :custom
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-auto t) ; Enables auto-completion
  (corfu-auto-prefix 2) 
  (corfu-auto-delay 0.15) ; Delay between typing and the completion window appearing
  (corfu-quit-at-boundry 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first nil)
  :bind (:map corfu-map
	      ("M-SPC" . corfu-insert-separator)
	      ("<tab>" . corfu-next)
	      ("<backtab>" . corfu-previous))
  :init
  ;; Use corfu everywhere
  (global-corfu-mode)
  ;; Save completion history for better sorting
  (corfu-history-mode))

;;; WHICH KEY
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order-reverse)
  :config
  (setq which-key-idle-delay 0.25))

;;; MAGIT
(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :config
  (setq magit-define-global-key-bindings 'recommended))
;;; THEMEING
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq use-dialog-box nil)
;; Set up catppuccin theme 
(use-package catppuccin-theme
  :init (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))
;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; LINE NUMBERS
(global-display-line-numbers-mode 1)
(dolist (mode '(;; org-mode-hook
                term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                eshell-mode-hook
                inferior-python-mode-hook
                helpful-mode-hook
                mu4e-view-mode-hook
                treemacs-mode-hook
                inferior-emacs-lisp-mode-hook
                doc-view-mode-hook
                image-minor-mode-hook
                pdf-tools-enabled-hook
                mu4e-main-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; OPACITY
(defvar gp/background-opacity 75
  "The default opacity of the background when the transparency mode is toggled on.")

;;;###autoload
(define-minor-mode gp/opacity-mode
  "Enables background frame opacity"
  :lighter " op"
  :global t
  (if gp/opacity-mode
      ;; Turn on opacity by setting the alppha value of the current
      ;; and all future frames
      (progn
	(set-frame-parameter nil 'alpha-background gp/background-opacity)
	(add-to-list 'default-frame-alist `(alpha-background . ,gp/background-opacity)))
    ;; Turn off the opacity otherwise
    (set-frame-parameter nil 'alpha-background 100)
    (assq-delete-all 'alpha-background default-frame-alist)))
(provide 'gp/opacity-mode)
;; Automatically enable transparency at launch
(gp/opacity-mode)

;;; CONFIG.EL AND BACKUP FILES LOCATION
(defvar config-path "~/.config/emacs"
  "The default path for emacs's configuration files within this config.")
(let ((custom-file-path (concat config-path "/custom.el")))
  (unless (file-exists-p custom-file-path)
    (write-region "" nil custom-file-path))
  (setq custom-file custom-file-path)
  (load custom-file))

(let ((backup-files-path (concat config-path "/backup-files")))
  (unless (file-directory-p backup-files-path)
    (mkdir backup-files-path))
  (setq
   backup-by-copying t ; don't fuck-up symlinks
   backup-directory-alist
   `(("." . ,backup-files-path)) ; Don't litter file system
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)) ; use versioned backups 
    
