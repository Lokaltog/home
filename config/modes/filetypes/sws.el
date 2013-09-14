(require-package 'jade-mode)
(require-package 'stylus-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-hook 'sws-mode-hook (lambda ()
                           (setq indent-tabs-mode t
                                 tab-width (default-value 'tab-width)
                                 sws-tab-width (default-value 'tab-width))))

(provide 'config/modes/filetypes/sws)
