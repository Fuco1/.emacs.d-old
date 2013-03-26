(use-package tex-site
  :load-path "site-lisp/auctex/"
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :commands TeX-latex-mode
  :config
  (progn
    (use-package latex
      :defer t
      :config
      (progn
        (require 'smartparens-latex)
        (use-package preview)
        (use-package font-latex)
        (fset 'tex-font-lock-suscript 'ignore)

        ;; fix italian quote highlight
        (push '("\"<" "\">") font-latex-quote-list)

        (defun my-latex-remove-command ()
          "Unwrap the expression that point is in or before, also
removing the command name.  By command we understand a symbol
starting with \\ and followed by a block of text enclosed in {}."
          (interactive)
          (let ((ok (sp-get-enclosing-sexp)))
            (cond
             ;; we're inside the { } block
             (ok
              (progn
                (save-excursion
                  (goto-char (sp-get ok :beg))
                  (zap-to-char -1 ?\\ ))
                (sp-splice-sexp)))
             ;; test if we are in looking at the command fromt he front
             ((looking-at "\\\\")
              (zap-up-to-char 1 ?{)
              (sp-unwrap-sexp))
             ;; otherwise we're inside the command name
             (t
              (zap-to-char -1 ?\\ )
              (zap-up-to-char 1 ?{)
              (sp-unwrap-sexp)))))
        (bind-key "C-c d" 'my-latex-remove-command LaTeX-mode-map)
        (bind-key "M-RET" 'LaTeX-insert-item LaTeX-mode-map)

        (defun my-LaTeX-mode-init ()
          (setq TeX-auto-save t)
          (setq TeX-parse-self t)
          (TeX-PDF-mode t)
          (setq reftex-plug-into-AUCTeX t)
          (setq TeX-source-correlate-mode t)
          (setq TeX-source-correlate-method 'synctex)
          (reftex-mode t)
          (TeX-fold-mode t)

          (keyadvice-mode t)
          (message "LaTeX mode init complete."))
        ;; ACUTeX replaces latex-mode-hook with LaTeX-mode-hook
        (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-init)

        (keyadvice-add-advice (kbd "`")
          (if (and (eq major-mode 'latex-mode) (texmathp))
              (let* ((events (let ((overriding-local-map LaTeX-math-keymap))
                               (read-key-sequence "math: ")))
                     (binding (lookup-key LaTeX-math-keymap events)))
                (call-interactively binding))
            keyadvice-do-it))))))
