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

(defun gp/configure-eshell ()
  (setq eshell-history-size 10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(defun eshell-other-window ()
  "Open `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))
;; Eshell
(use-package eshell
  :hook (eshell-first-time-mode . gp/configure-eshell)
  :bind
  ("C-c o e" . eshell)
  ("C-x 4 e" . eshell-other-window))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-did-you-mean
  :after eshell
  :config
  (eshell-did-you-mean-setup))

;; Allows visual commands ran in eshell to open in vterm
(use-package eshell-vterm
  :after eshell
  :config
  (eshell-vterm-mode))
