(defun my-isearch-forward-to-beginning ()
  "Repeat the forward search and place the point before the
matched text."
  (interactive)
  (isearch-repeat 'forward)
  (goto-char isearch-other-end))

(defun my-isearch-kill-match ()
  "Kill the text last matched by isearch."
  (interactive)
  (isearch-exit)
  (kill-region (point) isearch-other-end))

(defun my-isearch-zap-to-match ()
  "Kill the region between the point of origin of isearch and the
closest portion of the isearch match string.  The behaviour is
meant to be analogous to zap-to-char.  The deleted region does
not include the isearch word.  This is meant to be bound only in
isearch mode."
  (interactive)
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         (beg isearch-opoint))
    (cond
     ((< beg ismin)
      (goto-char ismin)
      (kill-region beg ismin))
     ((> beg ismax)
      (goto-char beg)
      (kill-region ismax beg))
     (t
      (error "Internal error in isearch kill function.")))
    (isearch-exit)))

(defun my-isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(defun my-isearch-yank-symbol ()
  "Pull next symbol from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (sp-forward-symbol) (point))))

;; keybindings
(global-set-key [(f6)] 'replace-regexp)
(global-set-key [(f7)] 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-v") 'my-isearch-forward-to-beginning)
(define-key isearch-mode-map (kbd "C-2") 'my-isearch-kill-match)
(define-key isearch-mode-map (kbd "C-3") 'my-isearch-exit-other-end)
(define-key isearch-mode-map (kbd "C-M-w") 'my-isearch-yank-symbol)
(define-key isearch-mode-map (kbd "M-z") 'my-isearch-zap-to-match)
