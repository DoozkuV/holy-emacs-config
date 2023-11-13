;;; UTILITY FUNCTIONS FOR DEALING WITH ARCH/PACMAN

;; NOTE: These functions are all run utilizing the yay package
;; which can be downloaded from the AUR
;; THEY WILL NOT WORK WITHOUT YAY INSTALLED

(defvar gp/sudo-program "sudo"
  "A string referring to the command to be used by arch package install commands")
(setq gp/sudo-program "doas")

(defvar gp/arch-use-yay t
  "Use yay for arch commands if installed")

(defun gp/arch-update ()
  "Runs the pacman/yay shell command to automatically update the system on Arch Linux"
  (interactive)
  (gp/arch-command "-Syyu" nil))

(defun gp/arch-install (program)
  "Runs the Yay shell command to install the inputted program"
  (interactive "MProgram Name: ")
  (gp/arch-command "-S" program))

(defun gp/arch-uninstall (program)
  "Runs the shell command to delete the inputted program"
  (interactive "MProgram Name: ")
  (gp/arch-command "-Rns" program))

(defun gp/arch-search (query)
  "Runs pacman -Ss utilizing the inputted query"
  (interactive "MQuery: ")
  (gp/arch-command "-Ss" query))

(defun gp/arch-query (query)
  "Runs pacman -Qs utilizing the inputted query"
  (interactive "MQuery: ")
  (gp/arch-command "-Qs" query))

(defun gp/arch-command (args programs)
  "Runs either arch or pacman with `gp/sudo-program', with the specified args and programs
If programs is nil, it will act as if nothing is there."
  (let ((pacman-executable (if (and (executable-find "yay") gp/arch-use-yay)
			       (format "yay --sudo %s" gp/sudo-program)
			     (format "%s pacman" gp/sudo-program))))
    (async-shell-command (concat pacman-executable " " args " " programs))))

(gp/leader-keys
  "a" '(:ignore t :which-key "arch")
  "au" '(gp/arch-update :which-key "Arch Update")
  "ai" '(gp/arch-install :which-key "Arch Install")
  "ad" '(gp/arch-uninstall :which-key "Arch Delete")
  "as" '(gp/arch-search :which-key "Arch Search")
  "aq" '(gp/arch-query :which-key "Arch Query"))

;; (keymap-global-set "C-c a u" 'gp/arch-update)
;; (keymap-global-set "C-c a i" 'gp/arch-install)
;; (keymap-global-set "C-c a d" 'gp/arch-uninstall)
;; (keymap-global-set "C-c a s" 'gp/arch-search)
;; (keymap-global-set "C-c a q" 'gp/arch-query)
