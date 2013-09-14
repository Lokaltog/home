(require-package 'magit)

(require-package 'git-commit-mode)
(require-package 'gitconfig-mode)
(require-package 'gitignore-mode)

(require-package 'git-gutter-fringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(setq git-gutter-fr:side 'right-fringe)

(provide 'config/git)
