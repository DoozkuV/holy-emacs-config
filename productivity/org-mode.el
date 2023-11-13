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
  :general
  ;; Local leader bindings
  (gp/local-leader-keys
    :keymaps 'org-mode-map
    "d" '(org-deadline :which-key "Insert Deadline")
    "b" '(org-babel-tangle :which-key "Babel Tangle")
    "i" '(org-insert-link :which-key "Insert Link")
    "y" '(org-store-link :which-key "Store Link")
    "q" '(org-set-tags-command :which-key "Set Tags")
    "e" '(org-export-dispatch :which-key "Export")
    "h" '(gp/org-toggle-emphasis-markers :which-key "Toggle Emphasis Markers")
    "x" '(org-toggle-checkbox :which-key "Toggle Emphasis Markers"))

  :hook
  (org-mode . flyspell-mode)
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
  ;; :after org
  :commands (org-roam-node-insert org-roam-node-find org-roam-capture)
  :general
  (gp/leader-keys
    "r" '(:ignore t :which-key "roam")
    "ri" '(org-roam-node-insert :which-key "Node Insert")
    "rc" '(org-roam-capture :which-key "Node Capture"))
  :config
  (setq org-roam-directory (file-truename (concat gp/org-directory "/roam")))
  (org-roam-db-autosync-mode))


;; Org-roam integration
(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :general
  (gp/leader-keys
    "rf" '(consult-org-roam-file-find :which-key "Node Find")

    "rl" '(consult-org-roam-backlinks :which-key "Find Roam Backlinks")
    "rL" '(consult-org-roam-forward-links :which-key "Find Roam Forward Links")

    "rs" '(consult-org-roam-search :which-key "Search in Roam")
    "rb" '(consult-org-roam-buffer :which-key "Search Roam Buffers")) 
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-."))
