;; The following code block creates a "custom.el" file if it doesn't exist
;; and sets it as the default location for customization variables
(let ((custom-file-path (config-path-concat "custom.el")))
  (unless (file-exists-p custom-file-path)
    (write-region "" nil custom-file-path))
  (setq custom-file custom-file-path)
  (load custom-file))

;; This code block creates a backup files directory if it doesn't exist
;; and sets it as the default directory for any backup files emacs makes
;; so that they don't annoyingly flood the file system 
(let ((backup-files-path (config-path-concat "backup-files")))
  (unless (file-directory-p backup-files-path)
    (mkdir backup-files-path))
  (setq
   backup-by-copying t ; don't fuck-up symlinks
   backup-directory-alist
   `(("." . ,backup-files-path)) ; Don't litter file system
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)) ; use versioned backups 
