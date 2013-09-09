(defadvice sgml-delete-tag (after reindent-buffer)
  (cleanup-buffer))

(defadvice sr-tabs-add (after remove-sunrise-from-name activate)
  (sr-tabs-rename (replace-regexp-in-string "(Sunrise)" "" (buffer-name))))

(defadvice kill-line (before kill-line-autoreindent activate)
  (if (member major-mode
              '(
                lisp-interaction-mode
                emacs-lisp-mode
                scheme-mode
                lisp-mode
                c-mode
                c++-mode
                latex-mode
                ))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(defadvice kill-visual-line (before kill-line-autoreindent activate)
  (if (member major-mode
              '(
                lisp-interaction-mode
                emacs-lisp-mode
                scheme-mode
                lisp-mode
                c-mode
                c++-mode
                latex-mode
                ))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(defadvice beginning-of-defun (before fix-org-navig activate)
  (when (eq major-mode 'org-mode) (previous-line)))

(defadvice 2C-dissociate (after close-window-after-disconnect activate)
  (delete-window))
