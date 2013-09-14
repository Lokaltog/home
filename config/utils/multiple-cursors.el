(require-package 'multiple-cursors)
(global-set-key (kbd "C-c C-m l") 'mc/edit-lines)
(global-set-key (kbd "C-c C-m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-m a") 'mc/mark-all-like-this)

(provide 'config/utils/multiple-cursors)
