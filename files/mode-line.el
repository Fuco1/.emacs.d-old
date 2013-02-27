;; set frame format
(setq-default
 frame-title-format
 '("%b  ;  %*"
   (:eval (when (buffer-file-name)
            (concat "  ;  "
                    (abbreviate-file-name default-directory))))
   " - Emacs"))

;; set line format
(setq-default
 mode-line-format
 '(" "
   ;; show only if the buffer is read-only or modified.
   (:eval
    (cond (buffer-read-only
           (propertize "RO " 'face 'font-lock-keyword-face))
          ((buffer-modified-p)
           (propertize "** " 'face 'mode-line-modified-status))
          (t "   ")))

   ;; cursor position & narrow info
   (:eval (when (buffer-narrowed-p)
            "Narrow "))
   (-3 "%p")
   " "
   (11 "(%l,%c)")

   ;; Path to the file in buffer. If it doesn't have associated file,
   ;; display nothing.
   (:propertize (:eval
                 (when buffer-file-name
                   (my-abbreviate-file-name default-directory)))
                face mode-line-secondary)

   ;; buffer name
   (:propertize "%b" face mode-line-buffer-id)

   ;; activated modes
   "    %[("
   mode-name
   mode-line-process
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-secondary))
   ")%]"

   ;; version control
   (vc-mode vc-mode)

   ;; see ~/.emacs.d/vendor/wc-mode/wc-mode.el
   (wc-mode
    (6 (" (wc:" (:eval (if (use-region-p)
                  (format "%d,%d,%d"
                          (abs (- (point) (mark)))
                          (count-words-region (point) (mark))
                          (abs (- (line-number-at-pos (point))
                                  (line-number-at-pos (mark)))))
                (format "%d,%d,%d"
                        (point-max)
                        (count-words-region (point-min) (point-max))
                        (line-number-at-pos (point-max)))))
        ")")
       ))

   " (" mode-line-mule-info ")"
   ))

(defface mode-line-secondary
  '((t (:foreground "#555753")))
  "Face used for displaying secondary content (minor modes, file path etc.)")

(defface mode-line-modified-status
  '((t (:inherit font-lock-warning-face :weight normal)))
  "Face used for modified status in mode line.")

(setq my-abbrev-file-name-alist
      `((,abbreviated-home-dir . "~/")
        ("^d:/Languages/" . ":L:")
        ("^d:/progs/emacs-24.1/lisp/" . ":E:")
        ("^d:/download/fic/" . ":FIC:")
        ("~/.emacs.d/elpa/" . ":ELPA:")
        ("~/.emacs.d/" . ":ED:")))

(setq cycbuf-file-name-replacements
      (mapcar (lambda (pair) (list (car pair) (cdr pair))) my-abbrev-file-name-alist))

(defun my-abbreviate-file-name (filename)
  "Shorten the FILENAME or directory according to
`my-abbrev-file-name-alist'. "
  (dolist (p my-abbrev-file-name-alist)
    (when (string-match (car p) filename)
      (setq filename (replace-match (cdr p) nil nil filename))))
  filename)
