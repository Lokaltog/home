;;; init.el --- Lokaltog's emacs configuration

;;; Commentary:

;; Main emacs configuration file.

;;; Code:

;; Init package archives
(require 'package)
(setq package-archives
      (nconc package-archives
             '(("melpa"     . "http://melpa.org/packages/")
               ("marmalade" . "http://marmalade-repo.org/packages/")
               ("org"       . "http://orgmode.org/elpa/"))))
(package-initialize)

;; Init use-package
(defun lokaltog-require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version) t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (lokaltog-require-package package min-version t)))))

(lokaltog-require-package 'use-package)
(require 'use-package)

;; Core configuration
(setq-default
 inhibit-splash-screen t
 inhibit-startup-message t
 initial-scratch-message nil
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
 blink-cursor-alist '((box . hbar))
 custom-file (expand-file-name "init-custom.el" user-emacs-directory)
 echo-keystrokes 0.02)

(fset 'yes-or-no-p 'y-or-n-p)

;; Hide all bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Enable disabled features
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Global built-in modes
(electric-indent-mode t)
(transient-mark-mode t)

(blink-cursor-mode t)
(column-number-mode t)
(line-number-mode t)

;; Set encoding to always be UTF-8
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Make inactive frames transparent
(set-frame-parameter (selected-frame) 'alpha '(100 80))
(add-to-list 'default-frame-alist '(alpha 100 80))

(load custom-file)

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Set font
(set-face-attribute 'default nil :family "Pragmata Pro" :height 110)

;; Highlight comment annotations
(defun lokaltog-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):?"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'lokaltog-font-lock-comment-annotations)

(defun lokaltog-set-tabs-mode-hook ()
  "Set tabs as indentation mode."
  (setq indent-tabs-mode t
        tab-width (default-value 'tab-width)
        sws-tab-width (default-value 'tab-width)
        sass-indent-offset (default-value 'tab-width)
        haml-indent-offset (default-value 'tab-width)
        jade-tab-width (default-value 'tab-width)))

(dolist (hook '(json nginx vcl sws text jade haml sws sass sql))
  (add-hook (intern (concat (symbol-name hook) "-mode-hook")) 'lokaltog-set-tabs-mode-hook))

;; Setup packages
(use-package diminish
  :ensure t)

(use-package subword
  :diminish subword-mode
  :config
  (global-subword-mode t))

(use-package server
  :if window-system
  :config
  (unless (server-running-p)
    (server-start)))

(use-package distinguished-theme
  :ensure t
  :config
  (load-theme 'distinguished t))

(use-package popwin
  :commands popwin-mode
  :config
  (popwin-mode 1)
  (push '("*Compile-Log*" :height 20 :noselect t) popwin:special-display-config))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package prog-mode
  :defer t)

(use-package prettify-symbols-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq prettify-symbols-alist
                    '(("<=" . ?)
                      (">=" . ?)
                      ("->" . ?)
                      ("<-" . ?)
                      ("=>" . ?)
                      ("!=" . ?)
                      ("==" . ?)
                      ("/=" . ?)
                      ("*=" . ?)
                      ("*=" . ?)
                      ("//" . ?)
                      ("/*" . ?)
                      ("/**" . ?)
                      ("*/" . ?)
                      (".." . ?)
                      ("..." . ?)
                      ("<<" . ?)
                      (">>" . ?)
                      ("lambda" . ?λ)
                      ))))
  (global-prettify-symbols-mode t))


(use-package guide-key
  :commands guide-key-mode
  :diminish guide-key-mode
  :ensure t
  :init
  (setq guide-key/guide-key-sequence '(","       ; Evil leader
                                       "g"
                                       "z"
                                       "C-h"     ; Help
                                       "C-x r"   ; Register commands
                                       "C-x 4"   ; Other window commands
                                       "C-x 5"   ; Other frame commands
                                       "C-x c"   ; Helm prefix
                                       "C-c")    ; Mode commands
        guide-key/idle-delay 0.1)
  (guide-key-mode 1))

(use-package evil
  :ensure t
  :init
  (defun def-assoc (key alist default)
    "Return cdr of `KEY' in `ALIST' or `DEFAULT' if key is no car in alist."
    (let ((match (assoc key alist)))
      (if match
          (cdr match)
        default)))
  (defun lokaltog-evil-cursor ()
    "Change cursor color according to evil-state."
    (let ((cursor-colors-default "#ffe326")
          (cursor-colors '((insert . "#a5e724")
                           (visual . "#b0b3ba")
                           (replace . "#ff4a52")))
          (cursor-types-default 'box)
          (cursor-types '((insert . 'bar)
                          (visual . 'box))))
      (setq cursor-type (def-assoc evil-state cursor-types cursor-types-default))
      (set-cursor-color (def-assoc evil-state cursor-colors cursor-colors-default))))
  (setq evil-default-cursor #'lokaltog-evil-cursor)
  :config
  (define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
  (define-key evil-visual-state-map (kbd "TAB") 'indent-region)

  (define-key evil-motion-state-map ";" 'evil-ex)
  (define-key evil-motion-state-map ":" 'evil-repeat-find-char)
  (evil-mode 1))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "B" 'projectile-switch-to-buffer
    "F" 'make-frame-command
    "f" 'projectile-find-file
    "t" 'projectile-find-tag
    "kb" 'kill-this-buffer
    "rt" 'projectile-regenerate-tags
    "p" 'projectile-switch-project))

(use-package god-mode
  :ensure t)

(use-package evil-god-state
  :ensure t
  :commands evil-execute-in-god-state
  :init
  (define-key evil-normal-state-map (kbd "SPC") 'evil-execute-in-god-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package whitespace
  :ensure t
  :diminish whitespace-mode global-whitespace-mode
  :init
  (setq whitespace-style (quote (face tabs newline tab-mark newline-mark))
        whitespace-display-mappings '((newline-mark ?\n [?· ?\n])
                                      (tab-mark     ?\t [?┊ ?\t] [?\\ ?\t])))
  :config
  (global-whitespace-mode t))

(use-package magit
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :init
  (setq git-gutter-fr:side 'right-fringe)
  :config
  (global-git-gutter-mode t))

(use-package flx-ido
  :ensure t)

(use-package ido-vertical-mode
  :ensure t)

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex))

(use-package ido
  :ensure t
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 20
        ido-use-faces nil
        ido-default-file-method 'selected-window)
  :config
  (defun lokaltog-bind-ido-keys ()
    "Keybindings for ido-mode."
    (define-key ido-completion-map (kbd "<backspace>") 'ido-delete-backward-word-updir))
  (add-hook 'ido-setup-hook 'lokaltog-bind-ido-keys)
  (flx-ido-mode t)
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-vertical-mode))

(use-package pos-tip
  :ensure t
  :init
  (setq x-gtk-use-system-tooltips nil))

(use-package flycheck-pos-tip
  :ensure t
  :init
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)

  ;; Fix tabbing issues
  (defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common)
      (indent-according-to-mode)))
  (define-key evil-insert-state-map (kbd "TAB") 'indent-or-complete)

  ;; Fix evil repeat action issue
  (mapc #'evil-declare-change-repeat
        '(company-complete-common
          company-select-next
          company-select-previous
          company-complete-selection
          ))

  ;; Navigate menu with C-n, C-p and tab
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)

    (define-key company-active-map [tab] 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)))

(use-package paren
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode t))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-globally-ignored-files (append projectile-globally-ignored-files '("waf" "'.lock-waf*'"))
        projectile-globally-ignored-directories (append projectile-globally-ignored-directories '("'.waf*'" "build" "out" "venv"))))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode +1))

(use-package spaceline-config
  :ensure spaceline
  :init
  (setq powerline-default-separator 'wave
        powerline-height 20
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-window-numbers-unicode t)
  :config
  (spaceline-spacemacs-theme))

(use-package smart-tabs-mode
  :ensure t
  :config
  (smart-tabs-insinuate 'c 'javascript)
  (defadvice align (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))
  (defadvice align-regexp (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it)))

(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :config
  (progn (add-to-list 'company-backends 'company-anaconda)))

(use-package company-anaconda
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq flycheck-jshintrc "~/.config/jshintrc"
        flycheck-flake8rc "~/.config/flake8"
        flycheck-indication-mode 'left-fringe)
  :config
  (global-flycheck-mode))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package uncrustify-mode
  :ensure t)

(use-package ace-jump-mode
  :ensure t
  :init
  (setq ace-jump-mode-move-keys (append "iduhetonasyfpg.c,r;lxbkmjwqv'z" nil)))

;; Filetype modes
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (setq indent-tabs-mode t
                                      tab-width (default-value 'tab-width)
                                      python-indent-offset (default-value 'tab-width))
                                (anaconda-mode)
                                (smart-tabs-mode-enable)
                                (smart-tabs-advice python-indent-line python-indent-offset))))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq js2-highlight-level 3
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

(use-package nginx-mode
  :ensure t
  :mode
  ("/etc/nginx/.*\\.conf\\'" . nginx-mode)
  (".*\\.nginx\\.conf\\'" . nginx-mode))

(use-package vcl-mode
  :ensure t
  :mode (".*\\.vcl\\'" . vcl-mode))

(use-package sql
  :ensure t
  :config
  (sql-highlight-postgres-keywords))

(use-package jade-mode
  :ensure t
  :mode (".*\\.jade\\'" . jade-mode))

(use-package sws-mode
  :ensure t)

(use-package stylus-mode
  :ensure t)

(use-package haml-mode
  :ensure t)

(use-package sass-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
