(use-package auto-complete-config
  :commands auto-complete-mode
  :config
  (progn
    (ac-config-default)
    (setq-default ac-sources
                  (append '(
                            ac-source-filename
                            ac-source-yasnippet
                            )
                          ac-sources))

    (setq ac-use-menu-map t)
    (bind-key "RET" 'popup-isearch-done popup-isearch-keymap)))
