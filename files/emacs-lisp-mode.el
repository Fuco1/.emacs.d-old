(setq my-emacs-lisp-open-line-list '(
                                    if
                                    when
                                    unless
                                    defun
                                    defmacro
                                    defvar
                                    defcustom
                                    let
                                    let*
                                    ))

(defun my-emacs-lisp-open-line ()
  "Opens a new line if the point is at the closing parens of
function on `my-emacs-lisp-open-line-list'."
  (interactive)
  (open-next-line)
  (when (save-excursion
          (forward-char)
          (backward-sexp)
          (member (car (sexp-at-point)) my-emacs-lisp-open-line-list))
    (newline)
    (indent-according-to-mode)
    (previous-line)))

(defun my-emacs-lisp-init ()
  (define-key emacs-lisp-mode-map (kbd "RET") 'my-emacs-lisp-open-line)
  )

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-init)
