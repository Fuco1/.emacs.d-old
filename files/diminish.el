(require 'diminish)
(diminish 'undo-tree-mode)
(diminish 'visual-line-mode)
(diminish 'smartparens-mode)
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode " δ"))
(eval-after-load 'projectile
  '(diminish 'projectile-mode))
(eval-after-load "face-remap"
  '(diminish 'buffer-face-mode))
(eval-after-load "reftex"
  '(diminish 'reftex-mode))
(eval-after-load "emmet-mode"
  '(diminish 'emmet-mode))
(eval-after-load "skeleton-complete"
  '(diminish 'skeleton-complete-mode))
(eval-after-load "golden-ratio"
  '(diminish 'golden-ratio-mode))
