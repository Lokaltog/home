(require 'mu4e)
(require 'smtpmail)
(setq mu4e-drafts-folder "/[Gmail].Drafts"
      mu4e-sent-folder "/[Gmail].Sent Mail"
      mu4e-refile-folder "/[Gmail].All Mail"
      mu4e-trash-folder "/[Gmail].Trash"
      mu4e-sent-messages-behavior 'delete
      mu4e-update-interval 300
      mu4e-get-mail-command "offlineimap"
      mu4e-headers-skip-duplicates t
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      mu4e-maildir-shortcuts '(("/INBOX" . ?i)
                               ("/[Gmail].Sent Mail" . ?s)
                               ("/[Gmail].Trash" . ?t)
                               ("/[Gmail].All Mail" . ?a))
      user-mail-address "kim.silkebaekken@gmail.com"
      user-full-name "Kim Silkebækken"
      message-signature (concat
                         "Med vennlig hilsen\n"
                         "Kim Silkebækken\n")
      message-send-mail-function 'smtpmail-send-it
      message-kill-buffer-on-exit t
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mu4e-headers-date-format "%Y-%m-%d"
      mu4e-headers-time-format "%H:%M"
      mu4e-headers-visible-lines 15)

(provide 'config/apps/mu4e)
