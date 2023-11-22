;;; LINE NUMBERS
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
;; Disable line numbers in the following modes 
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
		eww-mode-hook
                mu4e-main-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; RECENTF
(recentf-mode 1) ; Enable file history
(keymap-global-set "C-c C-r" 'recentf)

;;; REPEAT MODE
;; Allows one to repeat certain commands 
(repeat-mode 1)

;;; Kill read read only
(setq kill-read-only-ok t)

;;; STARTUP BUFFER
(setq inhibit-splash-screen t
      initial-buffer-choice nil)

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

;;; AUTO UPDATE EMACS PACKAGES
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  (auto-package-delete-old-versions t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;;; PDF READER
(use-package pdf-tools
  :init
  (pdf-loader-install))

;;; TRANSLATOR
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

;;; DICTIONARY
(setq dictionary-server "localhost")

;;; TELEGA - Telegram/Whatsapp Integration
;; (use-package telega)

;;; PASSWORD STORE
;; Integration with the pass program
(use-package password-store
  :defer)

;;; KILL ALL BUFFERS COMMAND
(defun gp/kill-all-buffers ()
  "Kills every buffer in the buffer list and then opens the scratch buffer."
  (interactive)
  (save-some-buffers)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))
(keymap-global-set "C-c C-k" 'gp/kill-all-buffers)
