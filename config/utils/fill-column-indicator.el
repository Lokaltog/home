(require-package 'fill-column-indicator)
(setq fci-rule-column 85
      fci-rule-width 1
      fci-rule-color "#282a2e")
(add-hook 'after-change-major-mode-hook 'fci-mode)

(provide 'config/utils/fill-column-indicator)
