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

;; global elisp settings
(eldoc-in-minibuffer-mode 1)

(defun my-emacs-lisp-init ()
  (with-map-bind-keys emacs-lisp-mode-map
    ("RET" 'my-emacs-lisp-open-line)
    ("C-M-;" 'clippy-describe-function)
    ("C-. ." 'my-describe-thing-in-buffer))

  (set-input-method "english-prog")
  (eldoc-mode 1)
  (letcheck-mode t))

(defun my-wrap-region (beg end)
  (goto-char end)
  (insert ")")
  (goto-char beg)
  (insert "("))

(defun my-goto-dominating-let ()
  "Find dominating let form"
  (while (and (> (car (syntax-ppss)) 0)
              (not (ignore-errors
                     (backward-up-list)
                     (save-excursion
                       (down-list)
                       (memq (symbol-at-point) '(let let*))))))))

(defun my-extract-to-let (name &optional arg)
  "Extract the form at point into a variable called NAME placed
in a let form ARG levels up.

If ARG is \\[universal-argument] place the variable into the most
inner let form point is inside of."
  (interactive "sName of variable: \nP")
  (let ((new-vform (sp-get (sp--next-thing-selection) (delete-and-extract-region :beg-prf :end)))
        (raw (sp--raw-argument-p arg))
        (arg (prefix-numeric-value arg)))
    (save-excursion
      (cond
       (raw
        (my-goto-dominating-let)
        (progn
          (down-list)
          (forward-sexp 2)
          (backward-down-list)
          (insert "\n(" name " " new-vform ")")
          (indent-according-to-mode)))
       (t
        (backward-up-list arg)
        (if (> arg 0)
            (sp-get (sp--next-thing-selection) (my-wrap-region :beg-prf :end))
          (insert "()")
          (backward-char))
        (insert "let ((" name " " new-vform "))\n")
        (backward-up-list)
        (indent-sexp))))
    (if (> arg 0)
        (insert name)
      (forward-sexp)
      (backward-down-list)
      (save-excursion
        (insert "\n")
        (backward-up-list)
        (indent-sexp)))))

(defun my-merge-let-forms ()
  "Merge the most inner let form into the next outer one."
  (interactive)
  (save-excursion
    (my-goto-dominating-let)
    (down-list)
    (forward-sexp 2)
    (backward-sexp)
    (let ((var-list (sp-get (sp--next-thing-selection) (delete-and-extract-region :beg-prf :end))))
      (sp-splice-sexp-killing-backward 1)
      (my-goto-dominating-let)
      (down-list)
      (forward-sexp 2)
      (backward-down-list)
      (insert "\n" var-list)
      (backward-down-list)
      (sp-unwrap-sexp)
      (backward-up-list 2)
      (indent-sexp))))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-init)
