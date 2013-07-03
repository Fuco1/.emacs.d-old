(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (progn
    (bind-key "C-c h" 'haskell-hoogle haskell-mode-map)
    (bind-key "C-c C-r" 'my-haskell-reload haskell-mode-map)

    (defun my-haskell-reload (&optional reload)
      (interactive)
      (inferior-haskell-load-file reload)
      (other-window 1))))
