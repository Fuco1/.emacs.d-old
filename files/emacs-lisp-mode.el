(require 'thingatpt)

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
                                    progn
                                    ))

(defun my-emacs-lisp-open-line ()
  "Opens a new line if the point is at the closing parens of
function on `my-emacs-lisp-open-line-list'."
  (interactive)
  (ignore-errors
    (my-newline)
    (when (and (save-excursion
                 (forward-char)
                 (backward-sexp)
                 (when (listp (sexp-at-point))
                   (memq (car (sexp-at-point)) my-emacs-lisp-open-line-list)))
               (thing-at-point 'sexp)
               (eq (following-char) ?\)))
      (newline)
      (indent-according-to-mode)
      (previous-line))))

(defun my-describe-thing-in-buffer ()
  "Display the full documentation of FUNCTION (a symbol) in the help buffer."
  (interactive)
  (let ((function (function-called-at-point))
        (variable (variable-at-point)))
    (cond
     ((/= variable 0) (describe-variable variable))
     (function (describe-function function)))))

(defun my-emacs-lisp-init ()
  (bind-key "RET" 'my-emacs-lisp-open-line emacs-lisp-mode-map)
  (bind-key "C-M-;" 'clippy-describe-function emacs-lisp-mode-map)
  (bind-key "C-M-;" 'clippy-describe-function lisp-interaction-mode-map)
  (bind-key "C-. ." 'my-describe-thing-in-buffer emacs-lisp-mode-map)
  (eldoc-mode 1)
  (letcheck-mode t))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-init)
