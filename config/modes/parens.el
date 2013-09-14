(require-package 'smartparens)
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit
      sp-autoskip-closing-pair 'always
      show-paren-delay 0)
(sp-use-paredit-bindings)
(smartparens-global-mode +1)
(show-paren-mode t)

(provide 'config/modes/parens)
