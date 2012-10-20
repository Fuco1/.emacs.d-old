;; insert a footnote to the text. The footnote is in a format
;; [i] Text of the footnote...
;; It will search for the first [i] in the text body and place \footnote{...} there
(fset 'fninsert
   [C-right C-right C-left ?\C-  ?\C-e ?\M-w ?\M-a ?\C-s ?\C-w ?\C-w right ?\M-< ?\C-s ?\C-s ?\C-  ?\C-\M-a delete ?\M-x ?f ?n ?o return])

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
