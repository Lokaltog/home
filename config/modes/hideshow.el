(require 'hideshowvis)
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode "hideshowvis" "Will indicate regions foldable with hideshow in the fringe." 'interactive)

(add-hook 'prog-mode-hook (lambda ()
                            (hs-minor-mode)
                            (hideshowvis-enable)
                            (hideshowvis-symbols)))

(provide 'config/modes/hideshow)
