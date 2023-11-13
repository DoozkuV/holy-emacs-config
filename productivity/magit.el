;;; MAGIT
;; See https://magit.vc/manual/magit/Global-Bindings.html for info regarding binds
(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :general
  (gp/leader-keys
    "g" '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "git status")
    "gg" '(magit :which-key "git open")
    "gd" '(magit-dispatch :which-key "git dispatch")
    "gf" '(magit-file-dispatch :which-key "git file dispatch")))

