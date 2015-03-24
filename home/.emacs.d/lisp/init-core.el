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
 blink-cursor-alist '((box . hbar))
 custom-file (expand-file-name "lisp/init-custom.el" user-emacs-directory))

(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(blink-cursor-mode t)
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
(set-face-attribute 'default nil :family "Pragmata Pro" :height 100)

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):?"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


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
            (lt/kw-add-pointers)))

(provide 'init-core)
;;; init-core.el ends here
