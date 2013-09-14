(require-package 'js2-mode)
(require-package 'json-mode)
(require-package 'flymake-jshint)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(setq js2-allow-keywords-as-property-names nil
      js2-highlight-level 3
      js2-strict-missing-semi-warning nil
      js2-strict-trailing-comma-warning nil)
(add-hook 'js2-mode-hook (lambda ()
                           (flymake-jshint-load)))

(provide 'config/modes/filetypes/js)
