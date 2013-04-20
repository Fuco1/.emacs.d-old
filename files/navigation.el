(use-package ace-jump-mode
  :bind (("C-\\" . ace-jump-mode))
  :config
  (progn
    (setq ace-jump-mode-scope 'window)))

(use-package jump-char
  :bind (("M-m" . jump-char-forward)))
