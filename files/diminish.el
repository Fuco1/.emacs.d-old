(require 'diminish)
(diminish 'yas-minor-mode)
(diminish 'undo-tree-mode)
(diminish 'visual-line-mode)
(diminish 'skeleton-complete-mode "SC")
(diminish 'smartparens-mode)
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode " δ"))
(eval-after-load 'projectile
  '(diminish 'projectile-mode))
(eval-after-load "face-remap"
  '(diminish 'buffer-face-mode))
(eval-after-load "reftex"
  '(diminish 'reftex-mode))
