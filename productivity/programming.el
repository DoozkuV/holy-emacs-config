;;; PROGRAMMING
;; Fish mode configuration
(use-package fish-mode
  :hook (fish-mode . (lambda ()
		       (add-hook 'before-save-hook 'fish_indent-before-save))))

;; Bash/Shell Scripting
;; Could also just use the lsp server, but
;; this should work just fine...
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; Yuck-mode for editing eww config files
(use-package yuck-mode
  :mode ("\\.yuck\\'" . yuck-mode))

;; Editting Yaml files
(use-package yaml-mode)

;; Haskel
(use-package haskell-mode)
;; Rust
;; There also exists a "Rustic" mode which adds more features
;; but for now we are going without 
(use-package rust-mode
  :config
  (setq rust-format-on-save t))

;; Editing markdown files
;; See https://github.com/jrblevin/markdown-mode for more info
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; (use-package paredit
;;   :hook
;;   ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . paredit-mode))

;;; LSP CONFIGURATION
;; We utilize the built in 'eglot' package that offers a more minimalist lsp
;; interface while taking advantage of much of emacs's existing functionality
(use-package eglot
  :bind (:map eglot-mode-map
	      ("C-c l f" . eglot-format)
	      ("C-c l r" . eglot-rename)
	      ("C-c l c a" . eglot-code-actions)
	      ("C-c l c o" . eglot-code-action-organize-imports)
	      ("C-c l c q" . eglot-code-action-quickfix)
	      ("C-c l c e" . eglot-code-action-extract)
	      ("C-c l c i" . eglot-code-action-inline)
	      ("C-c l c r" . eglot-code-action-rewrite))
  :general
  (gp/local-leader-keys
    :keymaps 'eglot-mode-map
    "f" '(eglot-format :which-key "Format")
    "r" '(eglot-rename :which-key "Eglot Rename")
    "c" '(:ignore t :which-key "code actions")
    "ca" '(eglot-code-actions :which-key "Code Actions")
    "co" '(eglot-code-action-organize-imports :which-key "Organize Imports")
    "cq" '(eglot-code-action-quickfix :which-key "Quickfix")
    "ce" '(eglot-code-action-extract :which-key "Extract")
    "ci" '(eglot-code-action-inline :which-key "Inline")
    "cr" '(eglot-code-action-rewrite :which-key "Rewrite"))
  :init
  (dolist (mode '(c++-mode-hook
		  c-mode-hook
		  java-mode-hook
		  rust-mode-hook
		  python-mode-hook))
    (add-hook mode 'eglot-ensure)))

(use-package yasnippet
  :diminish t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;;; OLD LSP CONFIGURATION
;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq read-process-output-max (* 1024 1024))
;;   (setq lsp-use-plists t)
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((c++-mode c-mode java-mode python-mode) . lsp-deferred)
;;   :config
;;   ;; Determines how often lsp-mode refreshes highlights, lenses, links, etc.
;;   (setq lsp-idle-delay 0.25)
;;   (lsp-enable-which-key-integration t))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :hook (lsp-mode . lsp-ui-mode))
