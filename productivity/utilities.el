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
(defun gp/run-command-other-buffer (key-sequence)
  "Runs a command that returns a buffer, and then opens that buffer in another window"
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence)))
    (cond
     ((null sym)
      (user-error "No command is bound to %s"
		  (key-description key-sequence)))
     ((commandp sym)
      (let ((buf (call-interactively sym)))
	(switch-to-buffer (other-buffer buf))
	(switch-to-buffer-other-window buf)))
     (t
      (user-error "%s is bound to %s which is not a command"
		  (key-description key-sequence)
		  sym)))))
;; Set keymap here to C-x 7
(keymap-global-set "C-x 7" 'gp/run-command-other-buffer)
