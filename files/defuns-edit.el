(require 'thingatpt)

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

(defun backward-up-list+ ()
  "Same as `backward-up-list', but can also move out from withing a string"
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
        (backward-char))
    (backward-up-list)))

(defun up-list+ ()
  "Same as `up-list', but can also move out from withing a string"
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

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation-logical-or-visual) (point)))
      (beginning-of-line-logical-or-visual)
    (back-to-indentation-logical-or-visual)))

(defun point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((face (plist-get (text-properties-at (point)) 'face)))
    (or (eq 'font-lock-comment-face face)
        (eq 'font-lock-comment-delimiter-face face))))

(defun back-to-visual-indentation ()
  "Move point to the first non-whitespace character on this visual line."
  (interactive)
  (interactive)
  (beginning-of-visual-line)
  (skip-syntax-forward " " (line-end-position))
  (backward-prefix-chars))

(defun back-to-indentation-logical-or-visual ()
  "Move back to logical or visual indentation."
  (interactive)
  (if (visual-line-mode) (back-to-visual-indentation) (back-to-indentation)))

(defun end-of-line-logical-or-visual ()
  "Move to the end of visual line if visual mode is enabled
   or to the logical end otherwise."
  (interactive)
  (if (visual-line-mode) (end-of-visual-line) (move-end-of-line)))

(defun beginning-of-line-logical-or-visual ()
  "Move to the beginning of visual line if visual mode is enabled
   or logical beginning otherwise"
  (interactive)
  (if (visual-line-mode) (beginning-of-visual-line) (move-beginning-of-line)))

(defun end-of-code-or-line ()
  "Move to the end of code. If already there, move to the end of line,
that is after the possible comment. If at the end of line, move to the
end of code.

Comments are recognized in any mode that sets syntax-ppss properly."
  (interactive)
  (let ((eoc (save-excursion
               (end-of-line-logical-or-visual)
               (while (and (point-in-comment)
                           (not (bolp))
                           (> (point) 1))
                 (backward-char))
               (skip-chars-backward " \t")
               (point))))
    (cond ((= (point) eoc)
           (end-of-line-logical-or-visual))
          (t
           (goto-char eoc)))))

(defun mc/mark-all-like-this-dwim (arg)
  "Uses some sane defaults to guess what the user want to do:

- If inside a defun, find and mark all the parts of current defun matchign
the currently active region. If no region is active, activate the word
under cursor.
- If in SGML/HTML mode and inside a tag, select the tag and its pair

With prefix, it behaves the same as original `mc/mark-all-like-this'"
  (interactive "P")
  (if arg
      (mc/mark-all-like-this)
    (let ((mode (with-current-buffer (current-buffer) major-mode)))
      (cond ((and (member mode '(sgml-mode html-mode))
                  (mc/mark-tags)) t)
            ((bounds-of-thing-at-point 'defun)
             (mc/select-under-cursor)
             (save-restriction
               (widen)
               (narrow-to-defun)
               (mc/mark-all-like-this)))
            (t (mc/select-under-cursor) (mc/mark-all-like-this))))))

(defun mc/select-under-cursor ()
  "Select the word under cursor"
  (interactive)
  (when (not (use-region-p))
    (let ((b (bounds-of-thing-at-point 'word)))
      (goto-char (car b))
      (set-mark (cdr b)))))

(defun mc/mark-tags ()
  "Mark the tag we're in and its pair for renaming."
  (interactive)
  (let ((context (car (last (save-excursion (sgml-get-context))))))
    (when (and context
               (> (point) (aref context 2))
               (< (point) (aref context 3)))
      (let* ((tag-position (aref context 1))
             (tag-length (length (aref context 4)))
             (main-start (- (aref context 3) 1 tag-length))
             (mirror-start (save-excursion
                             (if (eq tag-position 'open)
                                 (sgml-skip-tag-forward 1)
                               (sgml-skip-tag-backward 1)
                               (forward-sexp))
                             (- (point) 1 tag-length))))
        (goto-char main-start)
        (set-mark (+ main-start tag-length))
        (mc/save-excursion (goto-char mirror-start)
                           (push-mark (+ mirror-start tag-length))
                           (mc/create-fake-cursor-at-point))
        (mc/maybe-multiple-cursors-mode)))))

;; not finished!
(defun mc/tex-get-pair ()
  "Get the pair of the currently selected begin/end tag"
  (interactive)
  ;;  (save-excursion
  (skip-chars-backward "{A-Za-z0-9")
  (forward-char -1)
  (cond ((looking-at "\\\\begin{.*?}")
         (forward-char 1)
         (skip-chars-forward "A-Za-z0-9")
         (forward-char)
         (set-mark (point))
         (skip-chars-forward "A-Za-z0-9"))
        (t
         (error "Not inside \\begin{...}"))

        ;;   )
        ))
