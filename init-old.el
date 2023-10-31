;; Setup to make emacs startup significantly faster
;; This is later dialed back at the end of the config
(setq gc-cons-threshold (* 50 1000 1000))

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

;;; RECENTF
(recentf-mode 1) ; Enable file history
(keymap-global-set "C-c C-r" 'recentf)

;;; ELECTRIC PAIR MODE
;; Enable it for programming buffers 
(dolist (mode '(prog-mode-hook
		eshell-mode-hook
		vterm-mode-hook
		term-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (electric-pair-local-mode 1))))

;;; AUTO FILL MODE
(dolist (mode '(org-mode-hook
		text-mode-hook))
  (add-hook mode (lambda () (auto-fill-mode 1))))


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
;;; EMBARK
(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;;; AVY NAVIGATION
(use-package avy
  :custom
  (avy-keys '(?s ?a ?d ?f ?j ?k ?l ?e ?w ?c ?m ?p ?g ?h))
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g g" . avy-goto-line)
  ("M-g M-g" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

;;; WHICH KEY
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order-reverse)
  :config
  (setq which-key-idle-delay 0.25))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;;; MAGIT
;; See https://magit.vc/manual/magit/Global-Bindings.html for info regarding binds
(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-dispatch)
  ("C-c f" . magit-file-dispatch))

;;; TERMINAL CONFIGURATRION
;; Vterm
(use-package vterm
  :commands vterm
  :bind
  ("C-c o t" . vterm)
  ("C-x 4 t" . vterm-other-window)
  :config
  (setq vterm-shell "fish")
  (setq vterm-max-scrollback 10000))


;;; PROGRAMMING
;; Fish mode configuration
(use-package fish-mode
  :hook (fish-mode . (lambda ()
		       (add-hook 'before-save-hook 'fish_indent-before-save))))

;; Yuck-mode for editing eww config files
(use-package yuck-mode
  :mode ("\\.yuck\\'" . yuck-mode))

;;; ORG MODE
(defun gp/org-toggle-emphasis-markers ()
  "Toggles the 'org-hide-emphasis-markers' variable, effectively toggling whether or not to hide
emphasis markers inside of org mode"
  (interactive)
  (message "org-hide-emphasis-markers=%s"
	   (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))))

(defvar gp/org-directory "~/Documents/org"
  "The directory where this configuration's org files will be stored")
(use-package org
  :bind
  ("C-c o c" . org-capture)
  ("C-c o a" . org-agenda)
  :config
  ;; Make it so org mode always starts folded
  (setq org-startup-folded t)
  ;; Change how org folds display when minimized
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)

  ;; Basisc org agenda setup
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Where org mode looks for agenda files
  (setq org-agenda-files
	`(,gp/org-directory))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Custom org links are set here
  (setq org-link-abbrev-alist
	'(("spellwiki" . "http://dnd5e.wikidot.com/spell:")))
  ;; Custom todo keywords
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)
	  ("tasks.org" :maxlevel . 1)))

  ;; Template for org capture
  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp ,(concat gp/org-directory "/tasks.org") "Inbox")
	   "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
	  ("j" "Journal" entry
	   (file+olp+datetree ,(concat gp/org-directory "/journal.org"))
	   "* %<%I:%M %p> - Journal :journal:\n\n%?\n"
	   :clock-in :clock-resume
	   :empty-lines 1))))

;; Org roam configuration
(use-package org-roam
  :after org
  :commands (org-roam-node-insert org-roam-node-find org-roam-capture)
  :bind
  ("C-c r c" . org-roam-capture)
  ("C-c r f" . org-roam-node-find)
  ("C-c r i" . org-roam-node-insert)
  :config
  (setq org-roam-directory (file-truename (concat gp/org-directory "/roam")))
  (org-roam-db-autosync-mode))

;;; EMAIL
(use-package mu4e
  ;; Mu is a package installed /outside/ of emacs
  :ensure nil
  :bind
  ("C-c o m" . mu4e)
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t
        mu4e-use-maildirs-extension nil)

  ;; Referesh mail using isync every 10 minutes
  ;; NOTE: This is disabled in this config as this is being handled instead
  ;; by a bash script
  (setq mu4e-update-interval (* 10 60)
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir "~/.local/share/mail")
  ;; (auth-source-pass-enable)
  ;; (setq auth-source-do-cache nil
  ;; 	auth-sources '("~/.password-store/smtp.gmail.com/georgenpadron@gmail.com.gpg"))
  
    ;; Configure mail sending to use msmtp
  ;; (setq sendmail-program (executable-find "msmtp")
  ;;       send-mail-function #'smtpmail-send-it
  ;;       message-sendmail-f-is-evil t
  ;;       message-sendmail-extra-arguments '("--read-envelope-from")
  ;;       message-send-mail-function #'message-send-mail-with-sendmail)

  ;; Configuring SMTP to work properly with gmail
  (setq message-send-mail-function 'smtpmail-send-it
	starttls-use-gnutls t
	smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587)
  
  (setq mu4e-contexts
        (list
         ;; Personal Account
         (make-mu4e-context
          :name "Professional"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/georgenpadron@gmail.com" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "georgenpadron@gmail.com")
                  (user-full-name . "George N Padron")
                  ;; (smtpmail-smtp-server . "smtp.gmail.com")
                  ;; (smtpmail-smtp-service . 465)
                  ;; (smtpmail-stream-type . ssl)
                  (mu4e-drafts-folder . "/georgenpadron@gmail.com/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/georgenpadron@gmail.com/[Gmail]/Sent")
                  (mu4e-refile-folder . "/georgenpadron@gmail.com/[Gmail]/All Mail")
                  (mu4e-trash-folder . "/georgenpadron@gmail.com/[Gmail]/Trash")
                  (mu4e-maildir-shortcuts .
                                          (("/georgenpadron@gmail.com/INBOX" . ?i)
                                           ("/georgenpadron@gmail.com/[Gmail]/Sent Mail" . ?s)
                                           ("/georgenpadron@gmail.com/[Gmail]/Trash" . ?t)
                                           ("/georgenpadron@gmail.com/[Gmail]/Drafts" . ?d)
                                           ("/georgenpadron@gmail.com/[Gmail]/All Mail" . ?a)))
		  (smtpmail-mail-address . "georgenpadron@gmail.com")
		  (smtpmail-smtp-user . "georgenpadron@gmail.com")))

         ;; Wealth Account
         (make-mu4e-context
          :name "Wealth"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/wealth2005@gmail.com" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "wealth2005@gmail.com")
                  (user-full-name . "George N Padron")
                  ;; (smtpmail-smtp-server . "smtp.gmail.com")
                  ;; (smtpmail-smtp-service . 465)
                  ;; (smtpmail-stream-type . ssl)
                  (mu4e-drafts-folder . "/wealth2005@gmail.com/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/wealth2005@gmail.com/[Gmail]/Sent Mail")
                  (mu4e-refile-folder . "/wealth2005@gmail.com/[Gmail]/All Mail")
                  (mu4e-trash-folder . "/wealth2005@gmail.com/[Gmail]/Trash")
                  (mu4e-maildir-shortcuts .
                                          (("/wealth2005@gmail.com/INBOX" . ?i)
                                           ("/wealth2005@gmail.com/[Gmail]/Sent Mail" . ?s)
                                           ("/wealth2005@gmail.com/[Gmail]/Trash" . ?t)
                                           ("/wealth2005@gmail.com/[Gmail]/Drafts" . ?d)
                                           ("/wealth2005@gmail.com/[Gmail]/All Mail" . ?a)))
		  (smtpmail-mail-address . "wealth2005@gmail.com")
		  (smtpmail-auth-credentials .
		   '(("smtp.gmail.com" 587 "wealth2005@gmail.com" nil)))))

         ;; george.n.padron@vanderbilt.edu Account
         (make-mu4e-context
          :name "Vanderbilt"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/george.n.padron@vanderbilt.edu" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "george.n.padron@vanderbilt.edu")
                  (user-full-name . "George N Padron")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type . ssl)
                  (mu4e-drafts-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/Sent Mail")
                  (mu4e-refile-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/All Mail")
                  (mu4e-trash-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/Trash")
                  (mu4e-maildir-shortcuts .
                                          (("/george.n.padron@vanderbilt.edu/INBOX" . ?i)
                                           ("/george.n.padron@vanderbilt.edu/[Gmail]/Sent Mail" . ?s)
                                           ("/george.n.padron@vanderbilt.edu/[Gmail]/Trash" . ?t)
                                           ("/george.n.padron@vanderbilt.edu/[Gmail]/Drafts" . ?d)
                                           ("/george.n.padron@vanderbilt.edu/[Gmail]/All Mail" . ?a)))
                  		  (smtpmail-auth-credentials .
		   '(("smtp.gmail.com" 587 "georgenpadron@gmail.com" nil))))))))

;;; UTILITIES
;; PDF Reader
(use-package pdf-tools
  :init
  (pdf-loader-install))

(use-package go-translate
  :bind
  ("C-c o r" . gts-do-translate)
  :config
  (setq gts-translate-list '(("it" "en") ("en" "it")))

  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-bing-engine) (gts-google-engine))
         :render (gts-buffer-render))))

(use-package password-store
  :defer)

(defun gp/kill-all-buffers ()
  "Kills every buffer in the buffer list and then opens the scratch buffer."
  (interactive)
  (save-some-buffers)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))
(keymap-global-set "C-c C-k" 'gp/kill-all-buffers)

(defun gp/arch-update ()
  "Runs the Yay shell command to automatically update the system on Arch Linux"
  (interactive)
  (async-shell-command "yay -Syu"))
(keymap-global-set "C-c C-u" 'gp/arch-update)

(defun gp/arch-install (program)
  "Runs the Yay shell command to install the inputted program"
  (interactive "DProgram Name: ")
  (async-shell-command (format "sudo pacman -S %s" program)))

;;; THEMEING
;; This is at the end of the config because it is the least important
;; thing to perserve if something in this config breaks 
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq use-dialog-box nil)

;; Tab configuration
(setq tab-bar-show 1 ; Show tab bar only when more than 1 tab present
      tab-bar-new-button-show nil ; Disable new and cose button on tab bar
      tab-bar-close-button-show nil
      tab-bar-auto-width t) ; Static tab bar with if true
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

;; Bring back the GC Threshold for runtime performance
(setq gc-cons-threshold (* 2 1000 1000))
