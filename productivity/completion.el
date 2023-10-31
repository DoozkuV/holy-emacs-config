;;; COMPLETION CONFIGURATION
;; Most of the completion here centers around the use of the
;; 'Vertico' package, with other packages here to support it
;; Also included here are configurations for 'Corfu' and 'Embark'

;;; VERTICO COMPLETION
(use-package vertico
  :diminish
  :bind (:map vertico-map
	      ("C-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))
;; Save history of vertico between emacs session
(use-package savehist
  :init
  (savehist-mode))
;; Extra info in completion buffers
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Orderless fuzzy finding
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; CORFU COMPLETION
(use-package corfu
  :custom
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-auto t) ; Enables auto-completion
  (corfu-auto-prefix 2) 
  (corfu-auto-delay 0.15) ; Delay between typing and the completion window appearing
  (corfu-quit-at-boundry 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first nil)
  :bind (:map corfu-map
	      ("M-SPC" . corfu-insert-separator)
	      ("<tab>" . corfu-next)
	      ("<backtab>" . corfu-previous))
  :init
  ;; Use corfu everywhere
  (global-corfu-mode)
  ;; Save completion history for better sorting
  (corfu-history-mode))

;;; EMBARK
(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
