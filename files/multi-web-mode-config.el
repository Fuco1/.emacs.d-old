(use-package multi-web-mode
  :config
  (progn
    (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                      (javascript-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                      (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
    (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))))

(use-package sgml-mode
  :config
  (progn
    (defun my-html-mode-setup ()
      (multi-web-mode 1)
      (emmet-mode 1)
      (with-map-bind-keys html-mode-map
        ("C-c C-f" 'sp-html-next-tag)
        ("C-c C-b" 'sp-html-previous-tag)))
    (add-hook 'html-mode-hook 'my-html-mode-setup)))
