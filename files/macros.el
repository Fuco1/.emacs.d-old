;; insert a footnote to the text. The footnote is in a format
;; [i] Text of the footnote...
;; It will search for the first [i] in the text body and place \footnote{...} there
(fset 'fninsert
   [?\C-\M-f ?\C-f ?\C-  ?\M-e ?\M-w ?\M-a ?\C-s ?\C-w ?\C-w ?\C-s ?\C-s ?\C-m ?\C-  ?\C-\M-b delete ?\M-x ?f ?n ?o ?t ?e return])

(defun fnote ()
  (interactive)
  (insert "\\footnote{")
  (yank)
  (insert "}"))

;; swap between two last used buffers
(fset 'swap-buffer-to-last-used
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 98 return] 0 "%d")) arg)))
(global-set-key (kbd "C-x C-a") 'swap-buffer-to-last-used)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALIASES

(defalias 'qrr 'query-replace-regexp)
(defalias 'rs 'replace-string)
