(eval-after-load "haskell-mode"
  (setq haskell-program-name "ghci")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

  (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
  (setq haskell-hoogle-command "hoogle")

  (add-hook 'haskell-mode-hook (lambda() (define-key haskell-mode-map (kbd "C-c C-r") 'inferior-haskell-reload-file)))
  )
