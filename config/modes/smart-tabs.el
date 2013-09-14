(require-package 'smart-tabs-mode)
(smart-tabs-insinuate 'javascript)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  tab-width (default-value 'tab-width)
                  python-indent (default-value 'tab-width))
            (smart-tabs-mode-enable)
            ;; workaround, this needs to be python-indent-line instead of python-indent-line-1 which is the default advice
            (smart-tabs-advice python-indent-line python-indent)
            ))

(provide 'config/modes/smart-tabs)
