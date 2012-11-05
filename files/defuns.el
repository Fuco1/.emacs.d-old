;;; Personal functions

;; For loading libraries from the vendor directory
;; Modified from defunkt's original version to support autoloading.
;; http://github.com/defunkt/emacs/blob/master/defunkt/defuns.el
(defun vendor (library &rest autoload-functions)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (suffix (concat normal ".el"))
         (personal (concat "~/.emacs.d/files/" file))
   (found nil))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (set 'found t))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (set 'found t))
     ((file-exists-p suffix)  (set 'found t)))
    (when found
      (if autoload-functions
          (dolist (autoload-function autoload-functions)
            (autoload autoload-function (symbol-name library) nil t))
        (require library)))
    (when (file-exists-p (concat personal ".el"))
      (load personal))))

;; Quickly jump back and forth between matching parens/brackets
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Praesent libero orci, auctor sed, faucibus vestibulum, gravida vitae, arcu. Nunc posuere. Suspendisse potenti. Praesent in arcu ac nisl ultricies ultricies. Fusce eros. Sed pulvinar vehicula ante. Maecenas urna dolor, egestas vel, tristique et, porta eu, leo. Curabitur vitae sem eget arcu laoreet vulputate. Cras orci neque, faucibus et, rhoncus ac, venenatis ac, magna. Aenean eu lacus. Aliquam luctus facilisis augue. Nullam fringilla consectetuer sapien. Aenean neque augue, bibendum a, feugiat id, lobortis vel, nunc. Suspendisse in nibh quis erat condimentum pretium. Vestibulum tempor odio et leo. Sed sodales vestibulum justo. Cras convallis pellentesque augue. In eu magna. In pede turpis, feugiat pulvinar, sodales eget, bibendum consectetuer, magna. Pellentesque vitae augue."))

;; ;; Use the text around point as a cue what it is that we want from the
;; ;; editor. Allowance has to be made for the case that point is at the
;; ;; edge of a buffer.
;; (defun indent-or-expand (arg)
;;   "Either indent according to mode, or expand the word preceding
;; point."
;;   (interactive "*P")
;;   (if (and
;;        (or (bobp) (= ?w (char-syntax (char-before))))
;;        (or (eobp) (not (= ?w (char-syntax (char-after))))))
;;       (dabbrev-expand arg)
;;     (indent-according-to-mode)))

;; This override for transpose-words fixes what I consider to be a flaw with the
;; default implementation in simple.el. To traspose chars or lines, you always
;; put the point on the second char or line to transpose with the previous char
;; or line.  The default transpose-words implementation does the opposite by
;; flipping the current word with the next word instead of the previous word.
;; The new implementation below instead makes transpose-words more consistent
;; with how transpose-chars and trasponse-lines behave.
(defun transpose-words (arg)
  "[Override for default transpose-words in simple.el]
Interchange words around point, leaving point at end of
them. With prefix arg ARG, effect is to take word before or
around point and drag it backward past ARG other words (forward
if ARG negative).  If ARG is zero, the words around or after
point and around or after mark are interchanged."
  (interactive "*p")
  (if (eolp) (forward-char -1))
  (transpose-subr 'backward-word arg)
  (forward-word (+ arg 1)))

;; some functions to ease the work with mark and mark-ring
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; usefull mini calculator
(defun my-mini-calc (expr &optional arg)
  "Calculate expression

If ARG is given, then insert the result to current-buffer"
  (interactive
   (list (read-from-minibuffer "Enter expression: ")
     current-prefix-arg))

  (let ((result (calc-eval expr)))
    (if arg
    (insert result)
      (message (format "Result: [%s] = %s" expr result)))))

(defun fix-basic ()
  (interactive)
  (beginning-of-buffer)
  (replace-regexp "…" "\\\\dots ")
  (beginning-of-buffer)
  (replace-regexp "\\.\\.\\." "\\\\dots ")
  (beginning-of-buffer)
  (replace-regexp "\\\\dots[ ]+)" "\\\\dots)")
  (beginning-of-buffer)
  (replace-regexp "[[:space:]][[:space:]]+" " ")
  (beginning-of-buffer)
  (replace-regexp "[[:space:]]+--[[:space:]]+" "---")
  (beginning-of-buffer)
  (replace-regexp "[[:space:]]+–[[:space:]]+" "---")
  (beginning-of-buffer)
  (replace-regexp "[[:space:]]+-[[:space:]]+" "---")
  (beginning-of-buffer)
  (replace-regexp "“" "``")
  (beginning-of-buffer)
  (replace-regexp "„" "``")
  (beginning-of-buffer)
  (replace-regexp "”" "''")
  (beginning-of-buffer)
  (replace-regexp "‘" "`")
  (beginning-of-buffer)
  (replace-regexp "‚" "`")
  (beginning-of-buffer)
  (replace-regexp "’" "'")
  (beginning-of-buffer)
  (replace-regexp "~" "\\\\textasciitilde{}")
  (beginning-of-buffer)
  (replace-regexp "\\\\dots[ ]+''" "\\\\dots''")
  (beginning-of-buffer)
  (replace-regexp "\\([^\\]\\)%" "\\1\\\\%")
  (beginning-of-buffer)
  (replace-regexp "^[[:space:]]" "")
  (beginning-of-buffer)
  (replace-string "}\\emph{" "")
  (beginning-of-buffer)
  (replace-string "\\emph{}" "")
  (beginning-of-buffer)
  (query-replace-regexp "\\\\emph{ }" "")
  (beginning-of-buffer)
  (replace-regexp "


+" "

")
  (beginning-of-buffer)
  (replace-regexp "``-\\([^-]\\)" "``---\\1")
  (beginning-of-buffer)
  (replace-regexp "``---\\([^}]\\)" "\\\\dk{``---}\\1")
  (beginning-of-buffer) ; fix the hyphen at the end of quotation
  (replace-regexp "\\([^-]\\)-''" "\\1---''")
  (beginning-of-buffer)
  (query-replace-regexp "---''\\([^}]\\)" "\\\\dk{---''}\\1")
  (beginning-of-buffer) ; fix the hyphen before emph end
  (replace-regexp "\\([^-]\\)-}" "\\1---}")
  (beginning-of-buffer)
  (replace-regexp "!!+" "!")
  (beginning-of-buffer)
  (replace-regexp "\\?\\?+" "?")
  (beginning-of-buffer)
  (replace-regexp "\\([^-]\\)-
" "\\1---
")
  (beginning-of-buffer)
  (replace-regexp "``A\\([^}]\\)" "\\\\dk{``A}\\1")
)


(defun fix-quotes () ; and some less trivial quotation stuff
  (interactive)
  (beginning-of-buffer)
  (query-replace-regexp "[[:space:]]'\\([[:word:]]\\)" " `\\1")
  (beginning-of-buffer)
  (query-replace-regexp "``'\\([[:word:]]\\)" "```\\1")
  (beginning-of-buffer)
  (query-replace-regexp "^'" "`")
  (beginning-of-buffer)
  (replace-regexp ":[ ]``" ", ``")
  (beginning-of-buffer)
  (query-replace-regexp "\\([[:word:]]\\)-[ ]" "\\1\\\\dots ")
)

(defun fix-quotes-italian ()
  (interactive)
  (beginning-of-buffer)
  (replace-regexp "``" "\"<")
  (beginning-of-buffer)
  (replace-regexp "''" "\">")
  (beginning-of-buffer)
  (replace-regexp "“" "\"<")
  (beginning-of-buffer)
  (replace-regexp "”" "\">")
)

;; (beginning-of-buffer)
;; (replace-regexp "\\*\\*\\* \\*\\*\\* \\*\\*\\*" "\\\\Scene")

(defun fix-html (italic-class)
  (interactive "sClass name: ")
  (beginning-of-buffer)
  (replace-regexp "<p.*?>" "")
  (beginning-of-buffer)
  (replace-regexp "</p>" "

")
  (beginning-of-buffer)
  (replace-regexp
   (concat "<span class=\"" italic-class "\">\\(.*?\\)</span>")
   "\\\\emph{\\1}")
  (beginning-of-buffer)
  (replace-regexp " " " ")
  (beginning-of-buffer)
  (replace-regexp "<span.*?>" "")
  (beginning-of-buffer)
  (replace-regexp "</span>" "")
  (beginning-of-buffer)
  (replace-regexp "&ndash;" "–")
  (beginning-of-buffer)
  (replace-regexp "&bdquo;" "„")
  (beginning-of-buffer)
  (replace-regexp "&ldquo;" "“")
  (beginning-of-buffer)
  (replace-regexp "&#39;" "'")
  (beginning-of-buffer)
  (replace-regexp "&nbsp;" " ")
  (beginning-of-buffer)
  (replace-regexp "&hellip;" "…")
  (beginning-of-buffer)
  (replace-regexp "<.*?>" "")
)

(defun fix-html-fimfic ()
  (interactive)
  (beginning-of-buffer)
  (replace-regexp "</i><i>" "")
  (beginning-of-buffer)
  (replace-regexp "<i>\\(.*?\\)</i>" "\\\\emph{\\1}")
  (beginning-of-buffer)
  (replace-regexp "<br />" "
")
  (beginning-of-buffer)
  (replace-regexp "<hr />" "\\\\Scene")
  (beginning-of-buffer)
  (replace-regexp " " " ")
  (beginning-of-buffer)
  (replace-regexp "\\\\emph{ }" " ")
  (beginning-of-buffer)
  (replace-regexp "\\\\emph{}" "")
)


; &bdquo; -- open quote „
; &ldquo; -- "close quote" “
; &#39; -- '
; &nbsp; -- " " < space
; &hellip; -- …
