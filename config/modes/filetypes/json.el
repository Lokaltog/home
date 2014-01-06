(add-hook 'json-mode-hook (lambda ()
                            (setq indent-tabs-mode t
                                  tab-width (default-value 'tab-width)
                                  sws-tab-width (default-value 'tab-width))))

(provide 'config/modes/filetypes/json)
