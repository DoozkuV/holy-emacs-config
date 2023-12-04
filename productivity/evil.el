;;; EVIL CONFIGURATION
;; Yes, I know I wasn't going to do this. However, realistically this
;; is the best option for me. I like Vim bindings and I like Emacs - I
;; am willing to tolerate a lower max speed for now
(use-package evil
  :demand t ;; Prevent lazy loading
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t)
  ;; (setq evil-want-minibuffer t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-auto-indent t)
  (setq evil-lookup-func 'embark-act)

  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line))

(use-package general
  :config
  (general-create-definer gp/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

    (general-create-definer gp/local-leader-keys
      :states '(normal insert visual emacs)
      :prefix "SPC m"
      :non-normal-prefix "C-SPC m")

    (gp/leader-keys
      ;; Toggles
      "t" '(:ignore t :which-key "toggles")
      "ta" '(auto-fill-mode :which-key "Toggle auto fill")
      "tt" '(consult-theme :which-key "Choose Theme")
      "tc" '(corfu-mode :which-key "Toggle corfu")
      "tp" '(electric-pair-mode :which-key "Toggle electric pairs")
      "to" '(gp/opacity-mode :which-key "Toggle opacity")
      "tf" '(flyspell-mode :which-key "Toggle flyspell mode")
      "tF" '(flyspell-prog-mode :which-key "Toggle flyspell prog mode")
      "." '(find-file :which-key "Find Files")
      ">" '(find-file-other-window :which-key "Find Files Other Window")
      ;; Window Management
      "w" '(evil-window-map :which-key "window")
      ";" '(other-window-prefix :which-key "Display Buffer New Window")
      "`" '(evil-switch-to-windows-last-buffer
            :which-key "Switch To Last Buffer")

      ;; Buffer management
      "," '(consult-buffer :which-key "Switch Buffer")
      "<" '(consult-buffer-other-window :which-key "Switch Buffer Other Window")
      "b" '(:ignore t :which-key "buffer")
      ;;; Consult buffer
      "bb" '(consult-buffer :which-key "Kill Current Buffer")
      "bk" '(kill-current-buffer :which-key "Kill Current Buffer")
      "bK" '(gp/kill-all-buffers :which-key "Kill Buffer List")
      "bc" '(clone-buffer :which-key "Clone Buffer")
      "bx" '(scratch-buffer :which-key "Scratch Buffer")
      "bi" '(ibuffer :which-key "Ibuffer")
      "bs" '(switch-to-buffer :which-key "Switch Buffer")
      "bl" '(list-buffers :which-key "List Buffers")
      "br" '(revert-buffer :which-key "Revert Buffers")

      ;; Project management
      ;; NOTE: For some reason I can't get the 'project-prefix-map' to work properly
      ;; with this keybinding, so instead this simulate key is used instead. 
      "p" (general-simulate-key "C-x p" :which-key "project")
      "SPC" '(project-find-file :which-key "Find Project Files")
      "C-SPC" '(project-find-file :which-key "Find Project Files")

      ;; Open utilities
      "o" '(:ignore t :which-key "open")
      "oe" '(eshell :which-key "Open Eshell")
      "oc" '(org-capture :which-key "Open Org Capture")
      "x" '(scratch-buffer :which-key "Open Org Capture")
      "X" '(org-capture :which-key "Open Org Capture")
      "oC" '(calc :which-key "Open Calculator")
      "oa" '(org-agenda :which-key "Open Org Agenda")
      "ot" '(vterm :which-key "Open Terminal")
      "oT" '(vterm-other-window :which-key "Open Terminal")
      "oi" '(ielm :which-key "Open Ielm")
      "or" '(gts-do-translate :which-key "Open Translator")
      "oe" '(eshell :which-key "Open Eshell")
      "oE" '(eshell-other-window :which-key "Open Eshell")
      "op" '(treemacs :which-key "Open File-Tree")
      "om" '(mu4e :which-key "Open Mail")
      "ob" '(eww :which-key "Open Browser")
      ;; "j" '((lambda () (interactive) (org-capture nil "jj")) :which-key "Capture Journal")
      ;; "c" '((lambda () (interactive)
      ;;         (find-file (concat config-path "/config.org")))
      ;;       :which-key "Open Config")

      "cw" '(count-words :which-key "Count Words")
      "ct" '(gts-do-translate :which-key "Consult Translator")
      "cd" '(dictionary-search :which-key "Consult Dictionary")
      "cc" '(calc :which-key "Consult Calculator")
      ;; Help
      "h" '(help-command :which-key "help")

      ;; Search
      "s" '(:ignore t :which-key "search")
      "sr" '(consult-recent-file :which-key "Search Recent Files")
      "sb" '(consult-buffer :which-key "Search Buffers")
      "sg" '(consult-ripgrep :which-key "Search Grep")
      "sm" '(consult-man :which-key "Search Man")
      "si" '(consult-info :which-key "Search Info")
      "/" '(consult-line :which-key "Search By Buffer")

      ;; Quit
      "q" '(:ignore t :which-key "quit")
      "qr" '(restart-emacs :which-key "Restart Emacs")
      "qq" '(kill-emacs :which-key "Kill Emacs")))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-want-unimpaired-p t)
  ;; (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-org
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Set up comment commands
(use-package evil-nerd-commenter
  :general
  (general-define-key
   :states 'motion
   "gc" 'evilnc-comment-operator
   "gy" 'evilnc-yank-and-comment-operator))

;; Utility snipe command
(use-package evil-snipe
  :diminish
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  ; Set the scope of searches and repeated searches
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-repeat-scope 'visible)
  (setq evil-snipe-spillover-scope 'whole-visible))

;; Binds
;; y s <selection>: Insert delimiters
;; S: Same as above but for visual mode
;; c s: Change delimiters
;; d s: Delete delimiters
(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))
