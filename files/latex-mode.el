(eval-after-load "tex-mode"
  '(progn
     (fset 'tex-font-lock-suscript 'ignore)
     (define-key latex-mode-map (kbd "C-c d") 'my-latex-remove-command)))

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
