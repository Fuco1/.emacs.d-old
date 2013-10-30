;;; Personal functions

(defun lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Praesent libero orci, auctor sed, faucibus vestibulum, gravida vitae, arcu. Nunc posuere. Suspendisse potenti. Praesent in arcu ac nisl ultricies ultricies. Fusce eros. Sed pulvinar vehicula ante. Maecenas urna dolor, egestas vel, tristique et, porta eu, leo. Curabitur vitae sem eget arcu laoreet vulputate. Cras orci neque, faucibus et, rhoncus ac, venenatis ac, magna. Aenean eu lacus. Aliquam luctus facilisis augue. Nullam fringilla consectetuer sapien. Aenean neque augue, bibendum a, feugiat id, lobortis vel, nunc. Suspendisse in nibh quis erat condimentum pretium. Vestibulum tempor odio et leo. Sed sodales vestibulum justo. Cras convallis pellentesque augue. In eu magna. In pede turpis, feugiat pulvinar, sodales eget, bibendum consectetuer, magna. Pellentesque vitae augue."))

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

(defun view-clipboard ()
  (interactive)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (html-mode)
    (view-mode)))

(defsubst buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (- (point-max) (point-min)) (buffer-size)))

;;; function overloads
(eval-after-load "hi-lock"
  '(progn
     (defun hi-lock-read-face-name ()
       "Read face name from minibuffer with completion and history."
       (intern (completing-read
                "Highlight using face: "
                (mapcar 'symbol-name (face-list))
                nil
                nil
                "hi-"
                'face-name-history
                (car hi-lock-face-defaults))))))

;; By Stefan Monnier <foo at acm.org>.
(defun unfill-paragraph ()
  "Take a multi-line paragrap and make it into a single line of text.
This is the opposite of fill-paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun* my-list-interactive (&optional (file-name (buffer-file-name)))
  "Return a list of all interactive functions in file FILE-NAME."
  (loop for i in (cdr (assoc-string file-name load-history))
           if (and (consp i) (eq (car i) 'defun) (commandp (cdr i)))
           collect (cdr i)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun my-assimil--format-line (fill)
  (let ((fill-column 38)
        (fill-prefix fill))
    (fill-region (line-beginning-position) (line-end-position))))

(defun my-assimil-format (arg)
  (interactive "p")
  (dotimes (i arg)
    (my-assimil--format-line "     ")
    (forward-line -1)))

(defun my-assimil-format-ubung (arg)
  (interactive "p")
  (dotimes (i arg)
    (my-assimil--format-line "   ")
    (forward-line -1)))

(defun my-assimil-insert-dialog-template (arg)
  (interactive "p")
  (dotimes (i arg)
    (insert (format "%2d - \n" (1+ i)))))

(defun my-assimil-insert-ubung-template (arg)
  (interactive "p")
  (dotimes (i arg)
    (insert (format "%d. \n" (1+ i)))))
