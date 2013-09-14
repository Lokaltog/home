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

(provide 'config/utils/cursor-style)
