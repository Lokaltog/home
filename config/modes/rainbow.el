(require-package 'rainbow-mode)
(require 'rainbow-mode)

(add-hook 'after-change-major-mode-hook #'rainbow-mode)

(provide 'config/modes/rainbow)
