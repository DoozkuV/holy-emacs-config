;;; HELP INFO CONFIGURATION
;; Here is packages related to showing help messages or otherwise improving
;; the documentation of emacs

;;; WHICH KEY - Pop-up keybinds 
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order-reverse)
  :config
  (setq which-key-idle-delay 0.25))

;; HELPFUL - Better Help Buffers
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))
