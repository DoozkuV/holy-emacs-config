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
