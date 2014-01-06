(require-package 'autopair)

(setq autopair-autowrap t
      show-paren-delay 0)

(show-paren-mode t)
(autopair-global-mode)

(provide 'config/modes/parens)
