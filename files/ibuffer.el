(require 'ibuffer)
;; filter groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("emacs-config"
                (or (predicate
                     .
                     (let ((bfn (buffer-file-name (current-buffer))))
                       (when bfn
                         (and (string-match-p "\\.emacs\\.d" bfn)
                              (eq major-mode 'emacs-lisp-mode)))))))
               ("emacs"
                (or (mode . emacs-lisp-mode)
                    (mode . lisp-interaction-mode)
                    (mode . inferior-emacs-lisp-mode)))
               ("TeX"
                (or (mode . tex-mode)
                    (mode . plain-tex-mode)
                    (mode . latex-mode)))
               ("Markdown" (or (mode . markdown-mode)
                               (mode . gfm-mode)))
               ("Web"
                (or (mode . html-mode)
                    (mode . css-mode)
                    (mode . php-mode)
                    (mode . js-mode)))
               ("Dired"
                (mode . dired-mode))
               ("Images"
                (or (mode . image-dired-display-image-mode)
                    (mode . image-dired-thumbnail-mode)
                    (mode . image-mode)))
               ("Tramp"
                (or (name . "tramp")))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . c-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . cc-mode)
                 ;; etc
                 ))))))

;; (define-ibuffer-filter in-directory
;;   "Toggle current view to buffers whose default-directory is in QUALIFIER."
;;   (:description "in-directory"
;;    :reader (read-directory-name "Directory: "))
;;   (with-current-buffer buf (file-in-directory-p default-directory qualifier)))

(define-ibuffer-column size-h
  (:name "Size"
   :inline t
   :summarizer
   (lambda (column-strings)
     (let ((total 0))
       (dolist (string column-strings)
         (setq total
               ;; like, ewww ...
               (+
                (let ((number (float (string-to-number string))))
                  (cond
                   ((string-match-p "K" string)
                    (* number 1000))
                   ((string-match-p "M" string)
                    (* number 1000000))
                   (t number)))
                total)))
       (file-size-human-readable total 'si))))
  (file-size-human-readable (buffer-size) 'si))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified vc-status-mini read-only
              " " (name 25 25 :left :elide)
              " " (size-h 9 -1 :right)
              " " (mode 16 16 :left :elide)

              " " filename-and-process)
        (mark " " (name 16 -1)
              " " filename)))

;; startup function
(defun customize-ibuffer-mode ()
  "Startup function."
  (ibuffer-switch-to-saved-filter-groups "default")
  (add-to-list 'ibuffer-hidden-filter-groups "Tramp")
  (visual-line-mode -1)
  (toggle-truncate-lines 1))
(add-hook 'ibuffer-mode-hook 'customize-ibuffer-mode)

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent activate) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
