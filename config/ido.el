(require-package 'flx-ido)
(require-package 'ido-ubiquitous)
(require-package 'ido-vertical-mode)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filenam-at-point 'guess
      ido-max-prospects 20
      ido-default-file-method 'selected-window)

(flx-ido-mode t)
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-everywhere t)
(ido-vertical-mode)
(setq ido-use-faces nil)

(provide 'config/ido)
