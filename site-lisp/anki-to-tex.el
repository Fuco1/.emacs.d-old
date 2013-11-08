(defun my-dictionary-format ()
  (interactive)
  (fix-reset-after-each
    (replace-regexp "" "")
    (replace-regexp "" "")
    (replace-string "E'" "\\`E")
    (replace-regexp "~" "\\\\mytilde{}")
    (replace-regexp "intransitive" "$\\\\iota$")
    (replace-regexp "transitive" "$\\\\tau$")
    (replace-regexp "reflexive" "$\\\\rho$")
    (replace-regexp "auxiliary" "$\\\\alpha$")
    (replace-regexp "impersonal" "$\\\\mu$")
    (replace-regexp "<div><div>" "<div>")
    (replace-regexp "</div></div>" "</div>")
    (replace-regexp "&nbsp;" " ")
    (replace-regexp "&lt;" "<")
    (replace-regexp "&gt;" ">")
    (replace-string "< " "$<$ ")
    (replace-regexp "^<div>" "")
    (replace-regexp "\t</div>" "\t")
    (replace-regexp "<br />" "")
    (replace-regexp "(<i>" "<i>(")
    (replace-regexp "</i>)" ")</i>")
    (replace-regexp "<i>\\(.*?\\)</i>" "\\\\emph{\\1}")
    (replace-regexp "\\([^{]\\)(syn: \\(.*?\\))" "\\1\\\\emph{(syn: \\2)}")
    (replace-regexp "(syn: \\(.*?\\))" "{\\\\footnotesize ($\\\\sigma\\\\!\\\\!:\\\\!$ \\1)}")
    (replace-regexp "\\.\\.\\." "\\\\dots ")
    ;; remove all the divs in front of first tab
    (with-lines
      (goto-char (point-min))
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (save-excursion (re-search-forward "\t" nil t)))
          (replace-string "<div>" "")
          (replace-string "</div>" ""))))
    (while (re-search-forward "\t.*?<div>" nil t)
      (beginning-of-line)
      (re-search-forward "\t" nil t)
      (unless (looking-at "<div>")
        (insert "<div>")
        (re-search-forward "<div>" nil t)
        (backward-char 5)
        (insert "</div>"))
      (end-of-line)
      (unless (looking-back "</div>")
        (insert "</div>")))
    (while (re-search-forward "\t<div>" nil t)
      (backward-delete-char 5)
      (insert "
\\begin{enumerate}
\\item ")
      (end-of-line)
      (backward-delete-char 6)
      (insert "
\\end{enumerate}"))
    (replace-regexp "^\\(.*?\\)\t" "{\\\\bfseries \\1} ")
    (replace-regexp "</div><div>" "
\\\\item ")
    ;; what is this??
    (while (re-search-forward "\\\\item \\[" nil t)
      (backward-delete-char 7)
      (when (looking-back "^")
        (backward-delete-char 1))
      (insert "[")
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (save-excursion (re-search-forward "\\]" nil t)))
          (goto-char (point-min))
          (replace-regexp "
\\\\item " "; "))))
    (while (re-search-forward "\\[" nil t)
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (1- (save-excursion (re-search-forward "\\]" nil t))))
          (goto-char (point-min))
          (insert "{\\footnotesize ")
          (if (re-search-forward "= " nil t)
              (progn
                (backward-delete-char 2)
                (insert "\\emph{")
                (goto-char (point-max))
                (insert ";}}"))
            (goto-char (point-max))
            (insert ";}")))))
    (replace-regexp "\\[\\(.*?\\)\\]" "\\\\emph{\\1}")
    (let ((case-fold-search nil)) (replace-string "\\Bf" "\\bf"))
    (my-dictionary-fix-quotes)
    (replace-regexp "$" "
")
    (my-dictionary-remove-single-item-list)
    (replace-string "enumerate" "enumerate*")))

(provide 'anki-to-tex)