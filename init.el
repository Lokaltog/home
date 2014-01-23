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

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(package-initialize)

(dolist (file '(init-core
                init-hooks
                init-keymaps
                init-packages))
  (require file))
