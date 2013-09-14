(require-package 'nginx-mode)
(add-to-list 'auto-mode-alist '("/etc/nginx/.*\.conf$" . nginx-mode))

(provide 'config/modes/filetypes/nginx)
