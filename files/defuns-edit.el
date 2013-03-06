(require 'thingatpt)

(defun my-kill-whitespace (&optional forward)
  "Kill all the whitespace characters backwards until hitting
non-whitespace character.  With prefix argument, kill in the
forward direction."
  (interactive "P")
  (let ((old-point (point)))
    (if forward
        (skip-syntax-forward " ")
      (skip-syntax-backward " "))
    (delete-region old-point (point))))

(defun my-newline (&optional arg)
  "Call `newline' and autoindent according to the active mode."
  (interactive "p")
  (newline arg)
  (indent-according-to-mode))

(defun my-open-line (&optional arg)
  "If point is before the beginning of \"code\", open new line,
keep the cursor at the current line and autoindent.

If point is in the middle of the line, create a blank line under
current line, move cursor to this new line and autoindent.

With raw prefix \\[universal-argument] insert newline at point
and indent next line according to mode."
  (interactive "P")
  (if (or (<= (point) (save-excursion
                        (my-back-to-indentation)
                        (point)))
          (equal arg '(4)))
      (progn
        (save-excursion
          (my-newline (if (equal arg '(4)) 1 arg)))
        (indent-according-to-mode))
    (end-of-line)
    (open-line (prefix-numeric-value arg))
    (next-line 1)
    (indent-according-to-mode)))

(defun forward-line-and-indent (arg)
  "Move point ARG lines forward and autoindent."
  (interactive "p")
  (forward-line arg)
  (indent-according-to-mode))

(defun forward-paragraph-select ()
  "Set the active region from point to end of current paragraph"
  (interactive)
  (set-mark (point))
  (forward-paragraph))

(defun backward-paragraph-select ()
  "Set the active region from point to beginning of current paragraph"
  (interactive)
  (set-mark (point))
  (backward-paragraph))

(defun beginning-of-region ()
  "Move cursor to the beginning of active region"
  (interactive)
  (goto-char (region-beginning)))

(defun end-of-region ()
  "Move cursor to the end of active region"
  (interactive)
  (goto-char (region-end)))

;; from https://github.com/skeeto/.emacs.d/blob/master/my-funcs.el
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun expand-macro-and-replace ()
  "Replace the preceding sexp with its macroexpand."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (macroexpand (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun copy-line-with-offset (offset)
  "Save the line specified by offset (+1 = next, -1 = prev) to the kill ring,
move the current line down and yank"
  (kill-ring-save (line-beginning-position (+ offset 1))
                  (line-end-position (+ offset 1)))
  (let ((pos (point))
        (line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (beginning-of-line)
    (when (or (and (string-match "[:space:]" line)
                   (> offset 0))
              (< offset 0))
      (newline)
      (forward-line -1))
    (beginning-of-line)
    (insert (car kill-ring))
    (goto-char pos)))

(defun copy-previous-line ()
  (interactive)
  (copy-line-with-offset -1))

(defun copy-next-line ()
  (interactive)
  (copy-line-with-offset 1))

(defun point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((face (plist-get (text-properties-at (point)) 'face)))
    (or (eq 'font-lock-comment-face face)
        (eq 'font-lock-comment-delimiter-face face))))

(defun my-back-to-indentation ()
  (if visual-line-mode
      (flet ((beginning-of-line (arg) (beginning-of-visual-line arg)))
        (back-to-indentation))
    (back-to-indentation)))

(defun my-back-to-indentation-or-beginning (&optional arg)
  "Jump back to indentation of the current line.  If already
there, jump to the beginning of current line.  If visual mode is
enabled, move according to the visual lines."
  (interactive "p")
  (if (or (/= arg 1)
          (= (point) (save-excursion
                       (my-back-to-indentation)
                       (point))))
      (progn
        (if visual-line-mode
            (beginning-of-visual-line arg)
          (move-beginning-of-line arg))
        (when (/= arg 1)
          (my-back-to-indentation)))
    (my-back-to-indentation)))

(defun my-end-of-code-or-line (&optional arg)
  "Move to the end of code.  If already there, move to the end of line,
that is after the possible comment.  If at the end of line, move
to the end of code.

Example:
  (serious |code here)1 ;; useless commend2

In the example, | is the current point, 1 is the position of
point after one invocation of this funciton, 2 is position after
repeated invocation. On subsequent calls the point jumps between
1 and 2.

Comments are recognized in any mode that sets syntax-ppss
properly."
  (interactive "p")
  (flet ((end-of-line-lov () (if visual-line-mode
                                 (end-of-visual-line arg)
                               (move-end-of-line arg)))
         (beg-of-line-lov () (if visual-line-mode
                                 (beginning-of-visual-line arg)
                               (move-beginning-of-line arg))))
    (let ((eoc (save-excursion
                 (end-of-line-lov)
                 (while (and (point-in-comment)
                             (not (bolp)))
                   (backward-char))
                 (skip-syntax-backward " ")
                 ;; if we skipped all the way to the beginning, that
                 ;; means there's only comment on this line, so this
                 ;; should just jump to the end.
                 (if (= (point) (save-excursion
                                  (beg-of-line-lov)
                                  (point)))
                     (progn (end-of-line-lov)
                            (point))
                   (point)))))
      (if (= (point) eoc)
          (end-of-line-lov)
        (goto-char eoc)))))
