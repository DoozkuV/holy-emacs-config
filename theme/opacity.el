;;; OPACITY
(defvar gp/background-opacity 75
  "The default opacity of the background when the transparency mode is toggled on.")

;;;###autoload
(define-minor-mode gp/opacity-mode
  "Enables background frame opacity"
  :lighter " op"
  :global t
  (if gp/opacity-mode
      ;; Turn on opacity by setting the alppha value of the current
      ;; and all future frames
      (progn
	(set-frame-parameter nil 'alpha-background gp/background-opacity)
	(add-to-list 'default-frame-alist `(alpha-background . ,gp/background-opacity)))
    ;; Turn off the opacity otherwise
    (set-frame-parameter nil 'alpha-background 100)
    (assq-delete-all 'alpha-background default-frame-alist)))
(provide 'gp/opacity-mode)

;; Automatically enable transparency at launch
(gp/opacity-mode)
