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

(defadvice ispell-word (around fix-golden-ration activate)
  (golden-ratio-mode -1)
  ad-do-it
  (golden-ratio-mode 1))

(defadvice guide-key/popup-guide-buffer (around fix-golden-ration activate)
  (golden-ratio-mode -1)
  ad-do-it
  (golden-ratio-mode 1))

(defadvice quit-window (around fix-golden-ration activate)
  ad-do-it
  (golden-ratio))

(defadvice calendar-exit (around close-window activate)
  (let* ((wl (window-list))
         (cb (calendar-buffer-list))
         (wins-to-kill (mapcar (lambda (w) (cons (member (window-buffer w) cb) w)) wl)))
    ad-do-it
    (mapc (lambda (w) (when (car w) (delete-window (cdr w)))) wins-to-kill)))

(defadvice shell-command (around fix-encoding activate)
  (let ((coding-system-for-read 'cp1250))
    ad-do-it))
