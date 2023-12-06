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

(use-package lua-mode)

(use-package web-mode
  :mode ("\\.html\\'"
	 "\\.css\\'"
	 "\\.php\\'"))

;; (use-package paredit
;;   :hook
;;   ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . paredit-mode))

(use-package sly
  :mode ("\\.lisp\\'" . lisp-mode)
  :commands sly
  :config
  (setq inferior-lisp-program "sbcl"))

;;; SNIPPETS
(use-package yasnippet
  :diminish t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

