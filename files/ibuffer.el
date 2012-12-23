(require 'ibuffer)
;; filter groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("emacs-config"
                (or (filename . ".emacs.d")))
               ("emacs"
                (or (mode . emacs-lisp-mode)))
               ("TeX"
                (or (mode . tex-mode)
                    (mode . TeX-mode)
                    (mode . latex-mode)
                    (mode . LaTeX-mode)))
               ("Web"
                (or (mode . html-mode)
                    (mode . css-mode)
                    (mode . PHP-mode)))
               ("Sunrise"
                (or (mode . sr-mode)))
               ("Tramp"
                (or (name . "tramp")))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . c-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . cc-mode)
                 (mode . js-mode)
                 ;; etc
                 ))))))

;; column settings
(defun my-transform-buffer-name (name)
  "Transform buffer name for display in `ibuffer'."
  (cond
   ((string-match " *(Sunrise)" name)
    (substring name 0 (match-beginning 0)))
   (t name)))

(define-ibuffer-column name-trans
  (:inline t
   :header-mouse-map ibuffer-name-header-map
   :props
   ('mouse-face 'highlight 'keymap ibuffer-name-map
        'ibuffer-name-column t
        'help-echo '(if tooltip-mode
                "mouse-1: mark this buffer\nmouse-2: select this buffer\nmouse-3: operate on this buffer"
                  "mouse-1: mark buffer   mouse-2: select buffer   mouse-3: operate"))
   :summarizer
   (lambda (strings)
     (let ((bufs (length strings)))
       (cond ((zerop bufs) "No buffers")
         ((= 1 bufs) "1 buffer")
         (t (format "%s buffers" bufs))))))
  (propertize (my-transform-buffer-name (buffer-name))
              'font-lock-face (ibuffer-buffer-name-face buffer mark)))

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
  '((mark modified read-only
          " " (name-trans 25 25 :left :elide)
          " " (size-h 9 -1 :right)
          " " (mode 16 16 :left :elide)
          " " filename-and-process)
    (mark " " (name 16 -1)
          " " filename)))

;; startup function
(defun customize-ibuffer-mode ()
  "Startup function."
  (ibuffer-switch-to-saved-filter-groups "default")
  (add-to-list 'ibuffer-hidden-filter-groups "Sunrise")
  (add-to-list 'ibuffer-hidden-filter-groups "Tramp"))
(add-hook 'ibuffer-mode-hook 'customize-ibuffer-mode)

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent activate) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
