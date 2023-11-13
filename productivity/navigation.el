;;; NAVIGATION CONFIGURATION
;; Here are packages and config settings related to navigation
;; and movement through buffers and windows in Emacs

;;; AVY NAVIGATION
(use-package avy
  :custom
  (avy-keys '(?s ?a ?d ?f ?j ?k ?l ?e ?w ?c ?m ?p ?g ?h))
  :config 
  ;; Set general keybinds
  (general-define-key
   :states ''motion
   "g s" 'avy-goto-char-timer)
  (general-define-key
   :states ''motion
   :keymaps 'org-mode-map
   :major-modes 'org-mode
   "g h" 'avy-org-goto-heading-timer))
;; Old keybinds used without evil mode
;; :bind
;; ("C-:" . avy-goto-char)
;; ("C-'" . avy-goto-char-2)
;; ("M-g g" . avy-goto-line)
;; ("M-g M-g" . avy-goto-line)
;; ("M-g w" . avy-goto-word-1))
