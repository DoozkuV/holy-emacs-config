;;; KEYBINDS
;; This file sets some basic keybinds

;; Moves the cursor to end of line and creates a newline
(defun gp/move-cursor-to-newline ()
  "Inserts a newline and moves the cursor there"
  (interactive)
  (move-end-of-line 1)
  (newline 1 t))

;; (keymap-global-set "C-<return>" 'gp/move-cursor-to-newline)

;; Moves the cursor to start of line and creates a newline
(defun gp/move-cursor-to-prev-newline ()
  "Inserts a newline and moves the cursor there"
  (interactive)
  (move-beginning-of-line 1)
  (open-line 1))

;; (keymap-global-set "M-<return>" 'gp/move-cursor-to-prev-newline)
