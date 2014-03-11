;; set frame format
(setq-default
 frame-title-format
 '("%b ; %*"
   (:eval (when (buffer-file-name)
            (concat " ; "
                    (my-abbreviate-file-name default-directory (buffer-name)))))
   " - Emacs"))

;; set line format
(setq-default
 mode-line-format
 '(" "
   ;; show only if the buffer is read-only or modified.
   (:eval
    (cond (buffer-read-only
           (propertize "RO" 'face 'font-lock-keyword-face))
          ((buffer-modified-p)
           (propertize "**" 'face 'mode-line-modified-status))
          (t "  ")))

   ;; evil support
   (:eval (when (bound-and-true-p evil-mode)
            (evil-generate-mode-line-tag evil-state)))
   " "
   ;; cursor position & narrow info
   (:eval (when (buffer-narrowed-p)
            "Narrow "))
   (-3 "%p")
   " "
   (17 (:eval (if (use-region-p)
                  (format "(wc:%d,%d,%d)"
                          (abs (- (point) (mark)))
                          (count-words-region (point) (mark))
                          (abs (- (line-number-at-pos (point))
                                  (line-number-at-pos (mark)))))
                (format "(%%l,%%c,%d)" (point)))))

   ;; Path to the file in buffer. If it doesn't have associated file,
   ;; display nothing.
   (:propertize (:eval
                 (when buffer-file-name
                   (my-abbreviate-file-name default-directory (buffer-name))))
    face mode-line-secondary)

   ;; buffer name
   (:propertize (:eval
                 (if (tramp-tramp-file-p (buffer-name))
                     (substring (elt (tramp-dissect-file-name (buffer-name)) 3) 1)
                   (buffer-name)))
    face mode-line-buffer-id)

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
   " " global-mode-string

   ;; hack to make the modeline refresh after each change in buffer
   (:propertize "%l" face (:foreground "black" :height 0))
   ))

(setq-default 2C-mode-line-format mode-line-format)

(defface mode-line-secondary
  '((t (:foreground "#555753")))
  "Face used for displaying secondary content (minor modes, file path etc.)")

(defface mode-line-modified-status
  '((t (:inherit font-lock-warning-face :weight normal)))
  "Face used for modified status in mode line.")

(setq my-abbrev-file-name-alist
      `((,abbreviated-home-dir . "~/")
        ("^d:/languages/" . ":L:")
        ("^d:/progs/emacs-24.1/lisp/" . ":E:")
        ("^d:/download/fic/" . ":FIC:")
        ("^/media/Data/languages/" . ":L:")
        ("^/media/Data/progs/emacs-24.1/lisp/" . ":E:")
        ("^/media/Data/download/fic/" . ":FIC:")
        ("~/.emacs.d/elpa/" . ":ELPA:")
        ("~/.emacs.d/" . ":ED:")))

(setq cycbuf-file-name-replacements
      (mapcar (lambda (pair) (list (car pair) (cdr pair))) my-abbrev-file-name-alist))

(defun my-abbreviate-file-name (filename &optional buffer-name)
  "Shorten the FILENAME or directory according to
`my-abbrev-file-name-alist'. "
  (let* ((tramp-file-name (and (tramp-tramp-file-p filename)
                               (tramp-dissect-file-name filename)))
         (tramp-buffer-name (and (tramp-tramp-file-p buffer-name)
                                 (tramp-dissect-file-name buffer-name)))
         (real-buffer-name (or (and tramp-buffer-name
                                    (elt tramp-buffer-name 3))
                               buffer-name))
         (real-filename (or (and tramp-file-name
                                 (elt tramp-file-name 3))
                            filename)))
    (dolist (p my-abbrev-file-name-alist)
      (when (string-match (car p) real-filename)
        (setq real-filename (replace-match (cdr p) nil nil real-filename))))
    (let ((uniquified-name (s-chop-suffixes
                            (reverse (mapcar
                                      (lambda (x) (concat x "/")) (s-split "/" real-buffer-name)))
                            real-filename)))
      (if tramp-file-name
          (let ((fn (format "/%s:%s@%s:%s"
                            (elt tramp-file-name 0)
                            (elt tramp-file-name 1)
                            (elt tramp-file-name 2)
                            uniquified-name)))
            (if (s-ends-with-p "/" fn) fn
              (concat fn "/")))
        uniquified-name))))

(defvar minimal-mode-line-background "darkred"
  "Background colour for active mode line face when minimal minor
  mode is active")

(defvar minimal-mode-line-inactive-background "dim grey"
  "Background colour for inactive mode line face when minimal
  minor mode is active")

(defvar minimal-mode-line-height 0.1
  "Height of mode line when minimal minor mode is active")

(unless (facep 'minimal-mode-line)
  (copy-face 'mode-line 'minimal-mode-line))
(set-face-attribute 'minimal-mode-line nil
                    :background "darkred"
                    :height 0.1)
