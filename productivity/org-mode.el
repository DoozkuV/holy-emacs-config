;;; ORG MODE
(use-package org
  :ensure nil
  :preface
  (defvar gp/org-directory "~/Documents/org"
    "Directory of org files within this configuration")
  :hook
  (org-mode . flyspell-mode)
  :commands
  (org-timer-set-timer)
  :general
  (gp/local-leader-keys
    :keymaps 'org-mode-map
    "b" '(org-babel-tangle :which-key "Babel Tangle")
    "i" '(org-insert-link :which-key "Insert Link")
    "y" '(org-store-link :which-key "Store Link")
    "r" '(org-id-get-create :which-key "Generate ID for heading")
    "d" '(org-deadline :which-key "Set Deadline")
    "q" '(org-set-tags-command :which-key "Set Tags")
    "e" '(org-export-dispatch :which-key "Export")
    "l" '(org-latex-preview :which-key "Preview Latex")
    "h" '(gp/org-toggle-emphasis-markers :which-key "Toggle Emphasis Markers")
    "x" '(org-toggle-checkbox :which-key "Toggle Emphasis Markers"))
  ;; Open links with the enter key
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "RET" 'org-open-at-point)
  
  :config
  ;; Make it so org mode always starts folded
  (setq org-startup-folded 'showeverything)
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
	   "* TODO %?\n %U\n %i" :empty-lines 1)
	  ("n" "Notes")
	  ("na" "Algorithm Notes" entry
	   (file+olp+datetree ,(concat gp/org-directory "/notes/algorithms.org"))
	   "* %<%I:%M %p> - CS 3250 Algorithms :notes:\n\n%?\n")
	  ("np" "Progamming Languages Notes" entry
	   (file+olp+datetree ,(concat gp/org-directory
				       "/notes/programming-languages.org"))
	   "* %<%I:%M %p> - CS 3270 Programming Languages :notes:\n\n%?\n")
	  ("ng" "Geology Notes" entry
	   (file+olp+datetree ,(concat gp/org-directory
				       "/notes/geology.org"))
	   "* %<%I:%M %p> - EES 1510 Dynamic Earth: Intro Geology :notes:\n\n%?\n")
	  ("j" "Journal / Writing")
	  ("jm" "Musings Journal" entry
	   (file+olp+datetree ,(concat gp/org-directory "/journal/musings.org"))
	   "* %<%I:%M %p> - %^{Insert Name|Musing} :journal:\n\n%?\n"
	   :clock-in :clock-resume
	   :empty-lines 1)
	  ("jj" "Personal Journal" entry
	   (file+olp+datetree ,(concat gp/org-directory "/journal/journal.org"))
	   "* %<%I:%M %p> - Journal :journal:\n\n%?\n"
	   :clock-in :clock-resume
	   :empty-lines 1)))
  ;; Load org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  ;; Load exporting org-mode into markdown
  (require 'ox-md nil t))

;; Org roam configuration
(use-package org-roam
  ;; :after org
  :commands (org-roam-node-insert org-roam-node-find org-roam-capture)
  :general
  (gp/leader-keys
    "r" '(:ignore t :which-key "roam")
    "ri" '(org-roam-node-insert :which-key "Node Insert")
    "rf" '(consult-org-roam-file-find :which-key "Node Find")

    "rl" '(consult-org-roam-backlinks :which-key "Find Roam Backlinks")
    "rL" '(consult-org-roam-forward-links :which-key "Find Roam Forward Links")

    "rs" '(consult-org-roam-search :which-key "Search in Roam")
    "rb" '(consult-org-roam-buffer :which-key "Search Roam Buffers") 
    "rc" '(org-roam-capture :which-key "Node Capture")

    "rq" '(org-roam-tag-add :which-key "Add Filetags")
    "ru" '(org-roam-ui-open) :which-key "Open Roam UI")

  ;; Define a key for inserting text
  ;; Currently isn't working 
  ;; (general-define-key
  ;;  :states ''insert
  ;;  :keymaps 'org-roam-mode-map
  ;;  :major-modes 'org-mode
  ;;  "C-c i" 'org-roam-node-insert)
  :config
  (setq org-roam-directory (file-truename (concat gp/org-directory "/roam")))
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?" :target
	   (file+head "${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t))))


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
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-."))

;; For pretty org roam ui
(use-package websocket
  :after org-roam)
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))
