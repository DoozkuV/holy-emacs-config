;; Setup to make emacs startup significantly faster
;; This is later dialed back at the end of the config
(setq gc-cons-threshold (* 50 1000 1000))

;;; BASIC INITALIZATION
(setq user-mail-address "georgenpadron@gmail.com")
(global-auto-revert-mode 1) ; Revert buffers when underlying file changes
(setq global-auto-revert-non-file-buffers t) ; Revert dired and more

;;; USE-PACKAGE/PACKAGE MANAGER SETUP
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))


;;; LOAD CONFIGURATION FILES
;; Here I define some utilities which will come in use now and later
(defvar config-path "~/.config/emacs"
  "The default path for emacs's configuration files within this config.")

(defun config-path-concat (input)
  "Returns the input appended to the configuration path"
  (concat config-path "/" input))

;; Add in the load directory path
(load (config-path-concat "load-directory"))
(load-directory (config-path-concat "productivity"))
(load-directory (config-path-concat "theme")) ;; Load the theme after to avoid bugginess

;; Bring back the GC Threshold for runtime performance
(setq gc-cons-threshold (* 2 1000 1000))
