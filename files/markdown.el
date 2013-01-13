(defface markdown-language-keyword-face
  '((t (:inherit font-lock-keyword-face :family "Consolas")))
  "Face for langauge identifier in 'pre' text."
  :group 'markdown-faces)

(defvar markdown-language-keyword-face 'markdown-language-keyword-face
  "Face for language identifier in 'pre' text.")

(setq markdown-imenu-generic-expression
      '(("title"  "^\\(.*\\)[\n]=+$" 1)
        ("h2-"    "^\\(.*\\)[\n]-+$" 1)
        ("h1"     "^# \\(.*\\)$" 1)
        ("h2"     "^## \\(.*\\)$" 1)
        ("h3"     "^### \\(.*\\)$" 1)
        ("h4"     "^#### \\(.*\\)$" 1)
        ("h5"     "^##### \\(.*\\)$" 1)
        ("h6"     "^###### \\(.*\\)$" 1)
        ("fn"     "^\\[\\^\\(.*\\)\\]" 1)
        ))

(defun my-markdown-init ()
  (setq imenu-generic-expression markdown-imenu-generic-expression))

(add-hook 'markdown-mode-hook 'my-markdown-init)

(eval-after-load "markdown-mode"
  '(progn
     (define-key markdown-mode-map (kbd "S-<return>") 'my-markdown-newline)
     ;; add github flavored code block fontification
     (font-lock-add-keywords 'markdown-mode '((markdown-match-quoted-code-blocks
                                               .
                                               ((0 markdown-pre-face t t)
                                                (1 markdown-language-keyword-face t t)
                                                (2 markdown-pre-face t t)
                                                ))))
     ))

(defun markdown-match-quoted-code-blocks (last)
  "Match quoted code blocks from the point to LAST."
  (cond ((search-forward-regexp "^\\(```\\).*$" last t)
         (beginning-of-line)
         (let ((beg (point))
               (end (progn (end-of-line) (point))))
           (forward-line)
           (cond ((search-forward-regexp (match-string 1) last t)
                  (set-match-data (list beg (+ beg 3) (+ beg 3) end (1+ end) (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun my-markdown-newline ()
  "If we're inside a list, jump to next line and open up a new
item.  If it's a numbered list, insert the correct following
number."
  (interactive)
  (newline)
  (let (n)
    (if (not (save-excursion
               (previous-line)
               (beginning-of-line)
               (looking-at "[0-9]+")))
        (insert "* ")
      (setq n (1+ (string-to-int (match-string 0))))
      (insert (concat (int-to-string n) ". ")))))

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
      (next-line 2))))
