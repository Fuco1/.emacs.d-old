;; edits... in tex-font.el, change line
;;   "[^'\">ť]+"    ;a bit pessimistic
;; to enable better string highlight

(use-package "auctex"
  :file ("\\.tex\\'" . latex-mode)
  :config
  (progn
    (load "preview-latex.el" nil nil t)
    (fset 'tex-font-lock-suscript 'ignore)

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
    (bind-key "C-c d" 'my-latex-remove-command latex-mode-map)

    (bind-key "M-RET" 'LaTeX-insert-item LaTeX-mode-map)

    (defun my-LaTeX-mode-init ()
      (setq TeX-auto-save t)
      (setq TeX-parse-self t)
      (reftex-mode t)
      (TeX-fold-mode t))
    ;; ACUTeX replaces latex-mode-hook with LaTeX-mode-hook
    (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-init)

    (keyadvice-add-advice (kbd "`")
                          (if (and (eq major-mode 'latex-mode) (texmathp))
                              (let* ((events (let ((overriding-local-map LaTeX-math-keymap))
                                               (read-key-sequence "math: ")))
                                     (binding (lookup-key LaTeX-math-keymap events)))
                                (call-interactively binding))
                            keyadvice-do-it))))
