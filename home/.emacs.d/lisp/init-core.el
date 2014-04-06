;;; init-core.el --- Lokaltog's emacs configuration

;;; Commentary:

;; Main configuration file.

;;; Code:
(setq-default
 inhibit-splash-screen t
 inhibit-startup-message t
 backup-inhibited t
 auto-save-default nil
 create-lockfiles nil
 scroll-margin 10
 scroll-step 1
 scroll-conservatively 100000
 auto-window-vscroll nil
 indent-tabs-mode nil
 tab-width 4
 indicate-empty-lines t
 indicate-buffer-boundaries '((top . left) (bottom . left) (t . right))
 require-final-newline t
 next-line-add-newlines nil
 split-height-threshold nil
 split-width-threshold 0
 major-mode 'text-mode
 fill-column 85
 x-stretch-cursor t
 gc-cons-threshold 20000000
 vc-follow-symlink t
 custom-file (expand-file-name "lisp/init-custom.el" user-emacs-directory))

(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(column-number-mode t)
(line-number-mode t)

;; enable disabled features
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; global built-in modes
(electric-indent-mode t)
(global-subword-mode t)
(transient-mark-mode t)
(global-hl-line-mode t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(load custom-file)

;; set font
(set-face-attribute 'default nil :family "Pragmata Pro" :height 90)

;; add extra font lock keywords
(defun lt/kw-add-fixme ()
  "Add font lock keywords for FIXME/TODO/BUG tags in code."
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|BUG\\)\\>"
                             1 font-lock-warning-face t))))

(defun lt/kw-add-constant ()
  "Add font lock keywords for constants (all-uppercase strings in code)."
  (font-lock-add-keywords nil
                          '(("[^\"]\\<\\([A-Z_][A-Z0-9_]+\\)\\>[^(\"]"
                             1 font-lock-constant-face t))))

(defvar font-lock-number-face 'font-lock-number-face
  "Face name to use for numbers.")

(defface font-lock-number-face
  '((t (:foreground "orange")))
  "Font Lock mode face used to highlight numbers."
  :group 'font-lock-faces)

(defun lt/kw-add-numbers ()
  "Add font lock keywords for numbers."
  (font-lock-add-keywords nil '(;; Valid hex number (will highlight invalid suffix though)
                                ("\\b0x[[:xdigit:]]+[uUlL]*\\b" . font-lock-number-face)
                                ;; Invalid hex number
                                ("\\b0x\\(\\w\\|\\.\\)+\\b" . font-lock-warning-face)
                                ;; Valid floating point number.
                                ("\\(\\b[0-9]+\\|\\)\\(\\.\\)\\([0-9]+\\(e[-]?[0-9]+\\)?\\([lL]?\\|[dD]?[fF]?\\)\\)\\b"
                                 (1 font-lock-number-face) (3 font-lock-number-face))
                                ;; Invalid floating point number.  Must be before valid decimal.
                                ("\\b[0-9].*?\\..+?\\b" . font-lock-warning-face)
                                ;; Valid decimal number.  Must be before octal regexes otherwise 0 and 0l
                                ;; will be highlighted as errors.  Will highlight invalid suffix though.
                                ("\\b\\(\\(0\\|[1-9][0-9]*\\)[uUlL]*\\)\\b" 1 font-lock-number-face)
                                ;; Valid octal number
                                ("\\b0[0-7]+[uUlL]*\\b" . font-lock-number-face)
                                ;; Floating point number with no digits after the period.  This must be
                                ;; after the invalid numbers, otherwise it will "steal" some invalid
                                ;; numbers and highlight them as valid.
                                ("\\b\\([0-9]+\\)\\." (1 font-lock-number-face))
                                ;; Invalid number.  Must be last so it only highlights anything not
                                ;; matched above.
                                ("\\b[0-9]\\(\\w\\|\\.\\)+?\\b" . font-lock-warning-face))))

(defvar font-lock-pointer-face 'font-lock-pointer-face
  "Face name to use for C pointers.")

(defface font-lock-pointer-face
  '((t (:foreground "orange")))
  "Font Lock mode face used to highlight C pointers."
  :group 'font-lock-faces)

(defun lt/kw-add-pointers ()
  "Add font lock keywords for C pointers."
  (font-lock-add-keywords nil
                          '(("\\(\\*+\\)[a-z]"
                             1 font-lock-pointer-face t))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (lt/kw-add-numbers)
            (lt/kw-add-fixme)
            (lt/kw-add-constant)
            (lt/kw-add-pointers)))

(add-hook 'python-mode-hook
          (lambda ()
            (lt/kw-add-numbers)
            (lt/kw-add-fixme)
            (lt/kw-add-constant)))

(provide 'init-core)
;;; init-core.el ends here
