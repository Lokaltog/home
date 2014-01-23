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
 glasses-face (quote default)
 glasses-original-separator "_"
 glasses-separator "_"
 glasses-uncapitalize-p t
 x-stretch-cursor t
 custom-file "~/.emacs.d/lisp/init-custom.el")

(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(column-number-mode t)
(line-number-mode t)

;; global built-in modes
(electric-indent-mode t)
(global-subword-mode t)
(transient-mark-mode t)
(global-hl-line-mode t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(load custom-file)

(provide 'init-core)
