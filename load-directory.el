;;; This function
;;;###autoload 
(defun load-directory (directory)
  (require 'find-lisp)
  (mapcar (lambda (fn)
	    (load (file-name-sans-extension fn)))
	  (find-lisp-find-files directory "\\.el\\'")))
