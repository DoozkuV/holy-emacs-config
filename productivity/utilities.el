;;; BASIC INITALIZATION
(setq user-mail-address "georgenpadron@gmail.com")
(global-auto-revert-mode 1) ; Revert buffers when underlying file changes
(setq global-auto-revert-non-file-buffers t) ; Revert dired and more

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

;; ;;; SCRATCH BUFFER
;; (defun gp/scratch-buffer-defaults ()
;;   "Setup hooks to set defaults in the scratch buffer"
;;   (corfu-mode 0)
;;   (auto-fill-mode 1))
;; (advice-add 'scratch-buffer :after 'gp/scratch-buffer-defaults)

;;; ELECTRIC PAIR MODE
;; Enable it for programming buffers 
(dolist (mode '(prog-mode-hook
		eshell-mode-hook
		vterm-mode-hook
		term-mode-hook
		shell-mode-hook
		org-mode-hook))
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


(defun gp/elfeed-tube-fetch-from-clipboard ()
  "Runs 'elfeed-tube-fetch' on the contents of the clipboard"
  (interactive)
  (elfeed-tube-fetch (current-kill 0 t)))

;;; Elfeed
(use-package elfeed
  :general
  (gp/leader-keys
    "ow" '(elfeed :which-key "Open Elfeed"))
  :config
  (setq elfeed-feeds
	'("https://www.nhc.noaa.gov/xml/OFFNT3.xml"
	  "https://www.reutersagency.com/feed/?taxonomy=best-regions&post_type=best"
	  "https://www.reutersagency.com/feed/?best-topics=political-general&post_type=best")))

;; Elfeed youtube integration
(use-package elfeed-tube
  :after elfeed
  ;; :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))
(use-package elfeed-tube-mpv
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))


;;; TELEGA - Telegram/Whatsapp Integration
;; (use-package telega)

;;; PASSWORD STORE
;; Integration with the pass program
(use-package password-store
  :defer)

;;; Speed type - funny typing thing
(use-package speed-type
  :commands (speed-type-text)
  :config (evil-set-initial-state 'speed-type-mode 'insert))

;;; KILL ALL BUFFERS COMMAND
(defun gp/kill-all-buffers ()
  "Kills every buffer in the buffer list and then opens the scratch buffer."
  (interactive)
  (save-some-buffers)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))
(keymap-global-set "C-c C-k" 'gp/kill-all-buffers)
