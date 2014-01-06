(require-package 'js2-mode)
(require-package 'json-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(setq js2-allow-keywords-as-property-names nil
      js2-highlight-level 3
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil
      js2-auto-indent-p t
      js2-enter-indents-newline t
      js2-indent-on-enter-key t
      js2-indent-tabs-mode t
      js2-indent-level (default-value 'tab-width)
      js2-concat-multiline-strings 'eol
      js2-cleanup-whitespace t
      js2-include-node-externs t)

(provide 'config/modes/filetypes/js)
