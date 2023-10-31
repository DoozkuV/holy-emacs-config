;;; UTILITY FUNCTIONS FOR DEALING WITH ARCH/PACMAN

;; NOTE: These functions are all run utilizing the yay package
;; which can be downloaded from the AUR
;; THEY WILL NOT WORK WITHOUT YAY INSTALLED

;;;###autoload
(defun gp/arch-update ()
  "Runs the Yay shell command to automatically update the system on Arch Linux"
  (interactive)
  (async-shell-command "yay -Syu"))
(keymap-global-set "C-c C-u" 'gp/arch-update)

;;;###autoload
(defun gp/arch-install (program)
  "Runs the Yay shell command to install the inputted program"
  (interactive "MProgram Name: ")
  (async-shell-command (format "sudo pacman -S %s" program)))
