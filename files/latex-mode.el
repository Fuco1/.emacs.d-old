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
        (sp-local-pair 'latex-mode "\\begin" "\\end")
        (sp-local-tag 'latex-mode "\\ba" "\\begin{align*}" "\\end{align*}")

        (use-package preview)
        (use-package font-latex)
        (fset 'tex-font-lock-suscript 'ignore)

        (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
          (sp-local-pair "\\[" nil :post-handlers '(my-latex-math-block-indent)))

        (defun my-latex-math-block-indent (a action c)
          (when (eq action 'insert)
            (newline-and-indent)
            (save-excursion (newline))))

        (defun my-latex-compile ()
          (interactive)
          (save-buffer)
          (TeX-command "LaTeX" 'TeX-master-file nil))
        (bind-key "C-M-x" 'my-latex-compile LaTeX-mode-map)

        (defvar my-latex-wrap-choices '("emph"
                                        "textsc"))
        (defvar my-latex-wrap-history nil)

        (defun my-latex-wrap (macro-name)
          (interactive (list (ido-completing-read
                              "Macro> "
                              my-latex-wrap-choices
                              nil 'confirm nil my-latex-wrap-history)))
          (when (use-region-p)
            (let ((b (region-beginning))
                  (e (region-end)))
              (goto-char e)
              (insert "}")
              (goto-char b)
              (insert "\\" macro-name "{"))))
        (bind-key "C-c w" 'my-latex-wrap LaTeX-mode-map)

        (defun my-end-of-environment ()
          (interactive)
          (LaTeX-mark-environment)
          (end-of-region))

        (defun my-beginning-of-environment ()
          (interactive)
          (LaTeX-mark-environment)
          (beginning-of-region)
          (deactivate-mark))

        (bind-key "M-n" 'my-end-of-environment LaTeX-mode-map)
        (bind-key "M-p" 'my-beginning-of-environment LaTeX-mode-map)

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

        (defun my-LaTeX-preview-math ()
          (interactive)
          (let ((b (save-excursion (while (texmathp) (backward-char 1)) (1- (point))))
                (e (save-excursion (while (texmathp) (forward-char 1)) (point))))
            (preview-region b e)))
        (bind-key "C-<m-key>" 'my-LaTeX-preview-math preview-map)

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

          (LaTeX-add-environments
           '("derivation" LaTeX-env-label))
          (TeX-add-symbols '("emph" 1))

          (setq fill-column 100000)

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
