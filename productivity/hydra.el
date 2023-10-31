;;; HYDRA CONFIG
;; Hydra allows special keybind sets - here I create some simple hydra for basic functionality
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("n" text-scale-decrease "out")
  ("p" text-scale-increase "in")
  ("q" nil "finished" :exit t))
(keymap-global-set "C-c t s" 'hydra-text-scale/body)

(defhydra hydra-scale-window (:timeout 4)
  ("n" shrink-window "shrink vertically")
  ("p" enlarge-window "grow vertically")
  ("b" shrink-window-horizontally "shrink window horizontally")
  ("f" enlarge-window-horizontally "grow horizontally")
  ("q" nil "finished" :exit t))
(keymap-global-set "C-x 9" 'hydra-scale-window/body)
