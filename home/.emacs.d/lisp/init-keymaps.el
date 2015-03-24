;; evil mappings
(define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
(define-key evil-visual-state-map (kbd "TAB") 'indent-region)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;; swap :/;
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

;; smart jump to start of line
(defun beginning-of-line-or-text ()
  "Move to beginning of line or beginning of text"
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'beginning-of-line-or-text)

;; f key mappings
(global-set-key (kbd "<f9>") 'projectile-switch-to-buffer)
(global-set-key (kbd "<C-f9>") 'switch-to-buffer)
(global-set-key (kbd "<f10>") 'projectile-find-file)
(global-set-key (kbd "<C-f10>") 'projectile-switch-project)
(global-set-key (kbd "<f11>") 'projectile-find-tag)
(global-set-key (kbd "<C-f11>") 'projectile-regenerate-tags)

(provide 'init-keymaps)
