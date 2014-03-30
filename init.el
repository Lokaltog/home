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

;; util functions
(defun def-assoc (key alist default)
  "Return cdr of `KEY' in `ALIST' or `DEFAULT' if key is no car in alist."
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
      default)))

(defun lt/var-file (filename)
  (format "~/.var/%s" filename))

;; requires
(dolist (file '(init-core
                init-hooks
                init-packages
                init-keymaps))
  (require file))
