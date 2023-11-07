;;; RECENTF
(recentf-mode 1) ; Enable file history
(keymap-global-set "C-c C-r" 'recentf)

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

;;; WINDOW COMMANDS
;; The following function runs any command in another buffer
;;;###autoload
(defun gp/run-command-other-buffer (command)
  "Runs a command that returns a buffer, and then opens that buffer in another window"
  (interactive
   (list  (intern (read-extended-command))))
  (cond
   ((null command)
    (user-error "No command is bound to %s"
		(key-description key-sequence)))
   ((commandp command)
    (let ((buf (call-interactively command)))
      (switch-to-buffer (other-buffer buf))
      (switch-to-buffer-other-window buf)))
   (t
    (user-error "%s is bound to %s which is not a command"
		(key-description key-sequence)
		command))))

(defun gp/run-sequence-other-buffer (key-sequence)
  "Runs a command that returns a buffer, and then opens that buffer in another window"
  (interactive
   (list (read-key-sequence "Press key: ")))
  (gp/run-command-other-buffer (key-binding key-sequence)))

;; Set keymap here to C-x 7
(keymap-global-set "C-x 7 k" 'gp/run-sequence-other-buffer)
(keymap-global-set "C-x 7 f" 'gp/run-command-other-buffer) 
