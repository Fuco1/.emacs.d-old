(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (progn
    (defun -repeat (n x)
      "Return a list with X repeated N times.
Returns nil if N is less than 1."
      (let (ret)
        (--dotimes n (!cons x ret))
        ret))

    (defun my-markdown-toc ()
      "Generate table of content from # to ####### headers."
      (interactive)
      (let ((n 'nil)
            (last-m nil)
            (toc ""))
        (save-excursion
          (while (re-search-forward "^\\(#+\\) \\(.*\\)" nil t)
            (if (equal last-m (match-string 1))
                (progn
                  (setcar n (1+ (car n))))
              (if (< (length last-m) (length (match-string 1)))
                  (!cons 1 n)
                (!cdr n)
                (setcar n (1+ (car n))))
              (setq last-m (match-string 1)))
            (setq toc (concat
                       toc
                       (apply #'concat (-repeat (* 4 (1- (length (match-string 1)))) " "))
                       (int-to-string (car n))
                       ". ["
                       (match-string 2)
                       "]"
                       "(#"
                       (replace-regexp-in-string " " "-" (downcase (match-string 2)))
                       ")
"
                       ))))
        (insert toc)))

    (defun my-markdown-generate-anchors ()
      "Add anchors above each header.  If an anchor is present,
delete it and re-insert new one."
      (interactive)
      (let (m)
        (while (re-search-forward "^\\(#+\\) \\(.*\\)" nil t)
          (setq m (match-string 2))
          (beginning-of-line)
          (previous-line)
          (if (looking-at "<a")
              (delete-region (point) (line-end-position))
            (newline))
          (insert (concat
                   "<a name=\""
                   (replace-regexp-in-string " " "-" (downcase m))
                   "\" />"))
          (next-line 2))))))
