(defun lokaltog-sql-mode-hook ()
  (setq indent-line-function 'insert-tab
        indent-tabs-mode t
        tab-width (default-value 'tab-width))
  (sql-highlight-postgres-keywords))
(add-hook 'sql-mode-hook 'lokaltog-sql-mode-hook)

(provide 'config/modes/filetypes/sql)
