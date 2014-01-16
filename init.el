(require 'package)
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version) t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(setq package-archives
      (nconc package-archives
             '(("melpa"     . "http://melpa.milkbox.net/packages/")
               ("marmalade" . "http://marmalade-repo.org/packages/")
               ("elpa"      . "http://elpa.gnu.org/packages/")
               ("org"       . "http://orgmode.org/elpa/"))))

(setq load-path
      (nconc load-path
             '("~/.emacs.d/"
               "~/.emacs.d/packages/")))

(package-initialize)

(dolist (file '(core
                git
                hooks
                ido
                keymaps
                theme

                ;; app packages
                apps/erc
                apps/mu4e
                apps/org

                ;; basic modes
                modes/auto-complete
                modes/cua
                modes/golden-ratio
                modes/hideshow
                modes/parens
                modes/pretty-mode-plus
                modes/projectile
                modes/rainbow
                modes/rainbow-delimiters
                modes/smart-tabs
                modes/undo-tree
                modes/yasnippet

                ;; filetypes
                modes/filetypes/js
                modes/filetypes/json
                modes/filetypes/nginx
                modes/filetypes/python
                modes/filetypes/sql
                modes/filetypes/sws

                ;; utils
                utils/ack
                utils/change-inner
                utils/cursor-style
                utils/expand-region
                utils/flycheck
                utils/multiple-cursors
                utils/project-explorer
                utils/smex
                utils/uniquify
                utils/windmove))
  (require (intern (concat "config/" (symbol-name file)))))
