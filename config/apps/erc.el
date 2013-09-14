(require-package 'erc)
(setq erc-autojoin-channels-alist (quote (("emacs" "emacs")))
      erc-autojoin-delay 1
      erc-autojoin-mode t
      erc-email-userid "kim@silkebaekken.net"
      erc-nick "Lokaltog"
      erc-server "irc.freenode.org"
      erc-user-full-name "Kim Silkebækken")

(provide 'config/apps/erc)
