;; m-space joins lines as well as truncate whitespace
(global-set-key (kbd "M-SPC") (lambda () (interactive)(just-one-space -1)))

;; smart jump to start of line
(defun beginning-of-line-or-text ()
  "Move to beginning of line or beginning of text"
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'beginning-of-line-or-text)

(provide 'config/keymaps)
