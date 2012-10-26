;; line selection functions
(defun line-select ()
  "Set the active region to current line"
  (interactive)
  (back-to-indentation)
  (set-mark (point))
  (end-of-line))

(defun beginning-of-line-select ()
  "Set the active region from beginning of current line to the point"
  (interactive)
  (set-mark (point))
  (back-to-indentation))

(defun end-of-line-select ()
  "Set the active region from point to the end of current line"
  (interactive)
  (set-mark (point))
  (end-of-line))

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

(defun backward-up-list+ ()
  "Same as backward-up-list, but can also move out from withing a string"
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
        (backward-char))
    (backward-up-list)))

(defun up-list+ ()
  "Same as up-list, but can also move out from withing a string"
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
        (forward-char))
    (up-list)))

(defun copy-line ()
  "Copy line (from point to end) to the kill ring but restore it afterwards"
  (interactive)
  (save-excursion
    (let ((x (point)))
      (kill-line)
      (yank)
      (goto-char x))))

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

(defun mc/mark-all-like-this-dwim (arg)
  "Find and mark all the parts of current defun matchign the
currently active region. With prefix, mark occurences in whole buffer."
  (interactive "P")
  (if arg
      (mc/mark-all-like-this)
    (if (bounds-of-thing-at-point 'defun)
        (save-excursion
          (save-restriction
            (narrow-to-defun)
            (mc/mark-all-like-this)))
      (mc/mark-all-like-this))))
