;;; ORG MODE
(defun gp/org-toggle-emphasis-markers ()
  "Toggles the 'org-hide-emphasis-markers' variable, effectively toggling whether or not to hide
emphasis markers inside of org mode"
  (interactive)
  (message "org-hide-emphasis-markers=%s"
	   (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))))

(defvar gp/org-directory "~/Documents/org"
  "The directory where this configuration's org files will be stored")
(use-package org
  :bind
  ("C-c o c" . org-capture)
  ("C-c o a" . org-agenda)
  :config
  ;; Make it so org mode always starts folded
  (setq org-startup-folded t)
  ;; Change how org folds display when minimized
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)

  ;; Basisc org agenda setup
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Where org mode looks for agenda files
  (setq org-agenda-files
	`(,gp/org-directory))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Custom org links are set here
  (setq org-link-abbrev-alist
	'(("spellwiki" . "http://dnd5e.wikidot.com/spell:")))
  ;; Custom todo keywords
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)
	  ("tasks.org" :maxlevel . 1)))

  ;; Template for org capture
  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp ,(concat gp/org-directory "/tasks.org") "Inbox")
	   "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
	  ("j" "Journal" entry
	   (file+olp+datetree ,(concat gp/org-directory "/journal.org"))
	   "* %<%I:%M %p> - Journal :journal:\n\n%?\n"
	   :clock-in :clock-resume
	   :empty-lines 1))))

;; Org roam configuration
(use-package org-roam
  :after org
  :commands (org-roam-node-insert org-roam-node-find org-roam-capture)
  :bind
  ("C-c r c" . org-roam-capture)
  ("C-c r f" . org-roam-node-find)
  ("C-c r i" . org-roam-node-insert)
  :config
  (setq org-roam-directory (file-truename (concat gp/org-directory "/roam")))
  (org-roam-db-autosync-mode))
