(require-package 'smart-mode-line)
(require 'smart-mode-line)

(add-to-list 'sml/replacer-regexp-list '("^~/projects" ":proj:"))

(sml/setup)

(provide 'config/modes/smart-mode-line)
