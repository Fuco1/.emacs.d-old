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

;; TODO: if inside a comment, on enter should continue with comment
;; (call indent-new-comment-line was M-j)... or is this really useful?
(defun open-next-line ()
  "Instead of going to the beginning of line, autoindent according
to the active mode"
  (interactive)
  (newline)
  (indent-according-to-mode))

(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (if (looking-at "^")
      (open-line arg)
    (end-of-line)
    (open-line arg)
    (next-line 1)
    (indent-according-to-mode)))

(defun forward-line-and-indent (arg)
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

(defun my-back-to-indentation-or-beginning ()
  "Jump back to indentation of the current line.  If already
there, jump to the beginning of current line.  If visual mode is
enabled, move according to the visual lines."
  (interactive)
  (flet ((my-back-to-indentation
          ()
          (if (visual-line-mode)
              (flet ((beginning-of-line (arg) (beginning-of-visual-line arg)))
                (back-to-indentation))
            (back-to-indentation))))
    (if (= (point) (save-excursion
                     (my-back-to-indentation)
                     (point)))
        (if (visual-line-mode)
            (beginning-of-visual-line)
          (move-beginning-of-line))
      (my-back-to-indentation))))

(defun my-end-of-code-or-line ()
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
  (interactive)
  (flet ((end-of-line-lov () (if (visual-line-mode)
                                 (end-of-visual-line)
                               (move-end-of-line)))
         (beg-of-line-lov () (if (visual-line-mode)
                                 (beginning-of-visual-line)
                               (move-beginning-of-line))))
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
