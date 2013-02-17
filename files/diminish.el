(require 'diminish)
(diminish 'yas-minor-mode)
(diminish 'undo-tree-mode)
(diminish 'visual-line-mode)
(eval-after-load 'projectile
  '(diminish 'projectile-mode "PJ"))
(eval-after-load "face-remap"
  '(diminish 'buffer-face-mode))
