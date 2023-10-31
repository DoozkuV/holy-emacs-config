;;; EMAIL
(use-package mu4e
  ;; Mu is a package installed /outside/ of emacs
  :ensure nil
  :bind
  ("C-c o m" . mu4e)
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t
        mu4e-use-maildirs-extension nil)

  ;; Referesh mail using isync every 10 minutes
  ;; NOTE: This is disabled in this config as this is being handled instead
  ;; by a bash script
  (setq mu4e-update-interval (* 10 60)
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir "~/.local/share/mail")

  ;; Configuring SMTP to work properly with gmail
  (setq message-send-mail-function 'smtpmail-send-it
	starttls-use-gnutls t
	smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587)
  
  (setq mu4e-contexts
        (list
         ;; Personal Account
         (make-mu4e-context
          :name "Professional"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/georgenpadron@gmail.com" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "georgenpadron@gmail.com")
                  (user-full-name . "George N Padron")
                  (mu4e-drafts-folder . "/georgenpadron@gmail.com/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/georgenpadron@gmail.com/[Gmail]/Sent")
                  (mu4e-refile-folder . "/georgenpadron@gmail.com/[Gmail]/All Mail")
                  (mu4e-trash-folder . "/georgenpadron@gmail.com/[Gmail]/Trash")
                  (mu4e-maildir-shortcuts .
                                          (("/georgenpadron@gmail.com/INBOX" . ?i)
                                           ("/georgenpadron@gmail.com/[Gmail]/Sent Mail" . ?s)
                                           ("/Georgenpadron@gmail.com/[Gmail]/Trash" . ?t)
                                           ("/georgenpadron@gmail.com/[Gmail]/Drafts" . ?d)
                                           ("/georgenpadron@gmail.com/[Gmail]/All Mail" . ?a)))
		  (smtpmail-mail-address . "georgenpadron@gmail.com")
		  (smtpmail-smtp-user . "georgenpadron@gmail.com")))

         ;; Wealth Account
         (make-mu4e-context
          :name "Wealth"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/wealth2005@gmail.com" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "wealth2005@gmail.com")
                  (user-full-name . "George N Padron")
                  (mu4e-drafts-folder . "/wealth2005@gmail.com/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/wealth2005@gmail.com/[Gmail]/Sent Mail")
                  (mu4e-refile-folder . "/wealth2005@gmail.com/[Gmail]/All Mail")
                  (mu4e-trash-folder . "/wealth2005@gmail.com/[Gmail]/Trash")
                  (mu4e-maildir-shortcuts .
                                          (("/wealth2005@gmail.com/INBOX" . ?i)
                                           ("/wealth2005@gmail.com/[Gmail]/Sent Mail" . ?s)
                                           ("/wealth2005@gmail.com/[Gmail]/Trash" . ?t)
                                           ("/wealth2005@gmail.com/[Gmail]/Drafts" . ?d)
                                           ("/wealth2005@gmail.com/[Gmail]/All Mail" . ?a)))
		  (smtpmail-mail-address . "wealth2005@gmail.com")
		  (smtpmail-smtp-user . "wealth2005@gmail.com")))

         ;; george.n.padron@vanderbilt.edu Account
         (make-mu4e-context
          :name "Vanderbilt"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/george.n.padron@vanderbilt.edu" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "george.n.padron@vanderbilt.edu")
                  (user-full-name . "George N Padron")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type . ssl)
                  (mu4e-drafts-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/Sent Mail")
                  (mu4e-refile-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/All Mail")
                  (mu4e-trash-folder . "/george.n.padron@vanderbilt.edu/[Gmail]/Trash")
                  (mu4e-maildir-shortcuts .
                                          (("/george.n.padron@vanderbilt.edu/INBOX" . ?i)
                                           ("/george.n.padron@vanderbilt.edu/[Gmail]/Sent Mail" . ?s)
                                           ("/george.n.padron@vanderbilt.edu/[Gmail]/Trash" . ?t)
                                           ("/george.n.padron@vanderbilt.edu/[Gmail]/Drafts" . ?d)
                                           ("/george.n.padron@vanderbilt.edu/[Gmail]/All Mail" . ?a)))
		  (smtpmail-mail-address . "george.n.padron@vanderbilt.edu")
		  (smtpmail-smtp-user . "george.n.padron@vanderbilt.edu")))))
