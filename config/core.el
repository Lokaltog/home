(setq-default
 inhibit-splash-screen t
 inhibit-startup-message t
 backup-inhibited t
 auto-save-default nil
 scroll-margin 10
 scroll-step 1
 scroll-conservatively 100000
 auto-window-vscroll nil
 indent-tabs-mode nil
 tab-width 4
 indicate-empty-lines t
 indicate-buffer-boundaries "left"
 split-height-threshold nil
 split-width-threshold 0
 glasses-face (quote default)
 glasses-original-separator "_"
 glasses-separator "_"
 glasses-uncapitalize-p t
 custom-file "~/.emacs.d/config/custom.el")

(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-hl-line-mode t)
(global-subword-mode t)

(load custom-file)

(provide 'config/core)
