;;; NAVIGATION CONFIGURATION
;; Here are packages and config settings related to navigation
;; and movement through buffers and windows in Emacs

;;; AVY NAVIGATION
(use-package avy
  :custom
  (avy-keys '(?s ?a ?d ?f ?j ?k ?l ?e ?w ?c ?m ?p ?g ?h))
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g g" . avy-goto-line)
  ("M-g M-g" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))
