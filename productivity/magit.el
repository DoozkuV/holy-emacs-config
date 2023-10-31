;;; MAGIT
;; See https://magit.vc/manual/magit/Global-Bindings.html for info regarding binds
(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-dispatch)
  ("C-c f" . magit-file-dispatch))
