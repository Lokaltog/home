;; theme
(require-package 'monokai-theme)
(setq-default monokai-distinct-fringe-background t)
(load-theme 'monokai t)

;; whitespace-mode
(global-whitespace-mode t)

(setq-default whitespace-style (quote (face tabs newline tab-mark newline-mark))
              whitespace-display-mappings '(
                                            (newline-mark ?\n [?· ?\n])
                                            (tab-mark     ?\t [?┊ ?\t] [?\\ ?\t])
                                            ))

(set-face-attribute 'whitespace-tab nil
                    :foreground "#49483E"
                    :weight 'normal)
(set-face-attribute 'whitespace-newline nil
                    :foreground "#49483E"
                    :weight 'normal)

;; git packages
(require-package 'magit)
(require-package 'git-commit-mode)
(require-package 'gitconfig-mode)
(require-package 'gitignore-mode)
(require-package 'git-gutter-fringe)
(require 'git-gutter-fringe)

(global-git-gutter-mode t)
(setq git-gutter-fr:side 'right-fringe)

;; ido-mode
(require-package 'flx-ido)
(require-package 'ido-ubiquitous)
(require-package 'ido-vertical-mode)

(setq-default ido-enable-prefix nil
              ido-enable-flex-matching t
              ido-create-new-buffer 'always
              ido-use-filenam-at-point 'guess
              ido-max-prospects 20
              ido-use-faces nil
              ido-default-file-method 'selected-window)

(flx-ido-mode t)
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-everywhere t)
(ido-vertical-mode)

;; pretty symbols
(add-hook 'python-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  '(("<=" . ?≤)
                    (">=" . ?≥)
                    ("!=" . ?≠)
                    ("==" . ?≡)
                    (">>" . ?≫)
                    ("<<" . ?≪)
                    ("and" . ?∧)
                    ("or" . ?∨)
                    ("not in" . ?∉)
                    ("in" . ?∈)
                    ("not" . ?¬)
                    ("None" . ?∅)
                    ("def" . ?ƒ)
                    ("lambda" . ?λ)
                    ("return" . ?↩)
                    ))))

(global-prettify-symbols-mode t)

;; auto-complete
(require-package 'auto-complete)
(require 'auto-complete-config)

(setq ac-auto-show-menu t)
(global-auto-complete-mode t)

;; golden ratio window resizing
(require-package 'golden-ratio)
(golden-ratio-mode t)

;; handle parens
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in
        the echo area. Has no effect if the character before
        point is not of the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

(setq-default show-paren-delay 0)

(show-paren-mode t)

;; projectile
(require-package 'projectile)

(setq-default projectile-enable-caching t)
(projectile-global-mode)

;; rainbow delimiters
(require-package 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; rainbow-mode, colorizes color names in buffers
(require-package 'rainbow-mode)
(require 'rainbow-mode)

(add-hook 'after-change-major-mode-hook #'rainbow-mode)

;; smart mode line
(require-package 'smart-mode-line)
(require 'smart-mode-line)

(add-to-list 'sml/replacer-regexp-list '("^~/projects" ":proj:"))

(sml/setup)

;; smart tabs (indent with tabs, align with spaces)
(require-package 'smart-tabs-mode)
(smart-tabs-insinuate 'javascript)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  tab-width (default-value 'tab-width)
                  python-indent (default-value 'tab-width))
            (smart-tabs-mode-enable)
            ;; workaround, this needs to be python-indent-line instead of python-indent-line-1 which is the default advice
            (smart-tabs-advice python-indent-line python-indent)))

;; undo-tree
(require-package 'undo-tree)
(global-undo-tree-mode)

;; snippets
(require-package 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/"))
(yas-global-mode 1)

;; javascript modes and configuration
(require-package 'js2-mode)
(require-package 'json-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(setq-default js2-allow-keywords-as-property-names nil
              js2-highlight-level 3
              js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil
              js2-auto-indent-p t
              js2-enter-indents-newline t
              js2-indent-on-enter-key t
              js2-indent-tabs-mode t
              js2-indent-level (default-value 'tab-width)
              js2-concat-multiline-strings 'eol
              js2-cleanup-whitespace t
              js2-include-node-externs t)

(add-hook 'json-mode-hook (lambda ()
                            (setq indent-tabs-mode t
                                  tab-width (default-value 'tab-width)
                                  sws-tab-width (default-value 'tab-width))))

;; nginx config mode
(require-package 'nginx-mode)
(add-to-list 'auto-mode-alist '("/etc/nginx/.*\.conf$" . nginx-mode))

;; jedi (python completion)
(require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq-default jedi:setup-keys t
              jedi:complete-on-dot t)

;; override sql mode indentation
(defun lokaltog-sql-mode-hook ()
  (setq indent-line-function 'insert-tab
        indent-tabs-mode t
        tab-width (default-value 'tab-width))
  (sql-highlight-postgres-keywords))
(add-hook 'sql-mode-hook 'lokaltog-sql-mode-hook)

;; sws-modes
(require-package 'jade-mode)
(require-package 'stylus-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-hook 'sws-mode-hook (lambda ()
                           (setq indent-tabs-mode t
                                 tab-width (default-value 'tab-width)
                                 sws-tab-width (default-value 'tab-width))))

;; apps/erc
(require-package 'erc)
(setq-default  erc-autojoin-channels-alist (quote (("emacs" "emacs")))
               erc-autojoin-delay 1
               erc-autojoin-mode t
               erc-email-userid "kim@silkebaekken.net"
               erc-nick "Lokaltog"
               erc-server "irc.freenode.org"
               erc-user-full-name "Kim Silkebækken")

;; apps/mu4e
(require 'mu4e)
(require 'smtpmail)
(setq mu4e-drafts-folder "/[Gmail].Drafts"
      mu4e-sent-folder "/[Gmail].Sent Mail"
      mu4e-refile-folder "/[Gmail].All Mail"
      mu4e-trash-folder "/[Gmail].Trash"
      mu4e-sent-messages-behavior 'delete
      mu4e-update-interval 300
      mu4e-get-mail-command "offlineimap"
      mu4e-headers-skip-duplicates t
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      mu4e-maildir-shortcuts '(("/INBOX" . ?i)
                               ("/[Gmail].Sent Mail" . ?s)
                               ("/[Gmail].Trash" . ?t)
                               ("/[Gmail].All Mail" . ?a))
      user-mail-address "kim.silkebaekken@gmail.com"
      user-full-name "Kim Silkebækken"
      message-signature (concat
                         "Med vennlig hilsen\n"
                         "Kim Silkebækken\n")
      message-send-mail-function 'smtpmail-send-it
      message-kill-buffer-on-exit t
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mu4e-headers-date-format "%Y-%m-%d"
      mu4e-headers-time-format "%H:%M"
      mu4e-headers-visible-lines 15)

;; apps/org-mode
(require-package 'org)
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

;; ack-and-a-half
(require-package 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)

;; change inner/outer sections
(require-package 'change-inner)
(global-set-key (kbd "C-c C-i") 'change-inner)
(global-set-key (kbd "C-c C-o") 'change-outer)

;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
;; Valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type
(setq cursor-read-only-color "gray"
      cursor-read-only-cursor-type 'hbar
      cursor-overwrite-color "red"
      cursor-overwrite-cursor-type 'box
      cursor-normal-color "#eeeeee"
      cursor-normal-cursor-type 'bar)

(defun set-cursor-according-to-mode ()
  "Change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color cursor-read-only-color)
    (setq cursor-type cursor-read-only-cursor-type))
   (overwrite-mode
    (set-cursor-color cursor-overwrite-color)
    (setq cursor-type cursor-overwrite-cursor-type))
   (t
    (set-cursor-color cursor-normal-color)
    (setq cursor-type cursor-normal-cursor-type))))

(add-hook 'post-command-hook 'set-cursor-according-to-mode)

;; expand region when selecting
(require-package 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)

;; enable flycheck
(require-package 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; multiple cursors
(require-package 'multiple-cursors)

(global-set-key (kbd "C-c C-m l") 'mc/edit-lines)
(global-set-key (kbd "C-c C-m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-m a") 'mc/mark-all-like-this)

;; project explorer
;; TODO remove?
(require-package 'project-explorer)
(require 'project-explorer)

;; smex (m-x with ido-mode)
(require-package 'smex)

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; switch windows with shift+arrow keys
(require 'windmove)
(windmove-default-keybindings)

(provide 'init-packages)
