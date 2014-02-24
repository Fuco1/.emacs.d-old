;; this file contains redefinitions of the internals that are broken
;; on my system

;; /usr/local/share/emacs/24.3/lisp/net/browse-url.el.gz
(eval-after-load "browse-url"
  '(defun browse-url-can-use-xdg-open ()
     "Return non-nil if the \"xdg-open\" program can be used.
xdg-open is a desktop utility that calls your preferred web browser.
This requires you to be running either Gnome, KDE, Xfce4 or LXDE."
     t))

;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (if (and (elt state 2)
                (or (not (looking-at "\\sw\\|\\s_"))
                    (looking-at ":")))
           ;; car of form doesn't seem to be a symbol, or is a keyword
           (progn
             (if (not (> (save-excursion (forward-line 1) (point))
                         calculate-lisp-indent-last-sexp))
                 (progn (goto-char calculate-lisp-indent-last-sexp)
                        (beginning-of-line)
                        (parse-partial-sexp (point)
                                            calculate-lisp-indent-last-sexp 0 t)))
             ;; Indent under the list or under the first sexp on the same
             ;; line as calculate-lisp-indent-last-sexp.  Note that first
             ;; thing on that line has to be complete sexp since we are
             ;; inside the innermost containing sexp.
             (backward-prefix-chars)
             (current-column))
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state))))))))
