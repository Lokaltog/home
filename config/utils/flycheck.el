(require-package 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'config/utils/flycheck)
