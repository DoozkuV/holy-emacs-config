;;; HYDRA CONFIG
;; Hydra allows special keybind sets - here I create some simple hydra for basic functionality
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("q" nil "finished" :exit t))
(keymap-global-set "C-c t s" 'hydra-text-scale/body)

(defhydra hydra-scale-window (:timeout 4)
  ("j" enlarge-window "grow vertically")
  ("k" shrink-window "shrink vertically")
  ("h" shrink-window-horizontally "shrink window horizontally")
  ("l" enlarge-window-horizontally "grow horizontally")
  ("q" nil "finished" :exit t))
(keymap-global-set "C-x 9" 'hydra-scale-window/body)

(gp/leader-keys
  "ts" '(hydra-text-scale/body :which-key "Scale Text")
  "tw" '(hydra-scale-window/body :which-key "Scale Window"))
