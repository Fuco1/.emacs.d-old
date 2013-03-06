;;;_. Commentary & basic stuff
;; diredp modified stuff:
;; comment out:
;; in `diredp-font-lock-keywords-1'
;;   line about compressed files
;;   line about ignored files "\\|\\.\\(g?z\\|Z\\)[*]?\\)\\)$") ; Compressed.
;;                            "\\)\\)$")

;; external dependencies: bash in git d:/progs/git/bin/bash
;; we call find from bash to fix stupid windows * expansion

(use-package dired
  :bind ("C-x d" . my-dired-files)
  :init
  (progn
    (defun my-dired-files (&optional arg)
      "Like `ido-dired'.  With prefix argument call
`diredp-dired-files' with negative argument."
      (interactive "P")
      (if arg
          (progn
            (when (not (featurep 'icicles))
              (require 'icicles))
            (setq current-prefix-arg -1)
            (call-interactively 'diredp-dired-files))
        (ido-dired))))
  :config
  (progn
    ;; loads dired, dired-aux, dired-x
    (require 'dired+)

;;;_. Key bindings & hooks
    (defun my-image-dired-thumbnail-mode-init ()
      (bind-key "b" 'image-dired-backward-image image-dired-thumbnail-mode-map)
      (bind-key "f" 'image-dired-forward-image image-dired-thumbnail-mode-map)
      (bind-key "n" 'image-dired-next-line image-dired-thumbnail-mode-map)
      (bind-key "p" 'image-dired-previous-line image-dired-thumbnail-mode-map)
      (bind-key "q" 'kill-this-buffer image-dired-thumbnail-mode-map))
    (add-hook 'image-dired-thumbnail-mode-hook 'my-image-dired-thumbnail-mode-init)

    (defun my-image-dired-display-image-init ()
      (bind-key "q" 'kill-this-buffer image-dired-display-image-mode-map))
    (add-hook 'image-dired-display-image-mode-hook 'my-image-dired-display-image-init)

    (defun my-dired-init ()
      "Bunch of stuff to run for dired, either immediately or when it's loaded."
      (bind-key "<insert>" 'dired-mark dired-mode-map)
      (bind-key "SPC" 'dired-mark dired-mode-map)
      (bind-key "<delete>" 'dired-unmark-backward dired-mode-map)
      (bind-key "<backspace>" 'dired-up-directory dired-mode-map)

      (bind-key "C-s" 'dired-isearch-filenames dired-mode-map)
      (bind-key "ESC C-s" 'dired-isearch-filenames-regexp dired-mode-map)

      (bind-key "C-o" 'dired-omit-mode dired-mode-map)

      (bind-key "/" 'my-dired-filter-by-regexp dired-mode-map)
      (bind-key "(" 'dired-details-toggle dired-mode-map)

      (bind-key "M-<f5>" 'my-dired-zip-files dired-mode-map)
      (bind-key "M-<f6>" 'my-dired-unpack-files dired-mode-map)

      (dired-omit-mode t))
    (add-hook 'dired-mode-hook 'my-dired-init)

    (defun my-dired-filter-by-regexp (regexp)
      (interactive "sRegexp: ")
      (let ((cbuffer (current-buffer)))
        (dired-mark-files-regexp regexp)
        (call-interactively 'diredp-marked)
        (with-current-buffer cbuffer
          (dired-unmark-all-marks))))

;;;_. Sorting
    (require 'ls-lisp)

    ;; redefine this function, to fix the formatting of file sizes in dired mode
    (defun ls-lisp-format-file-size (file-size human-readable)
      (if (or (not human-readable)
              (< file-size 1024))
          (format (if (floatp file-size) " %11.0f" " %11d") file-size)
        (do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
             ;; kilo, mega, giga, tera, peta, exa
             (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
            ((< file-size 1024) (format " %10.0f%s"  file-size (car post-fixes))))))

    (defvar dired-sort-modes-list
      '(("size" "S" "")
        ("ext" "X" "S")
        ("cdate" "ct" "X")
        ("date" "t" "ct")
        ("name" "" "t"))
      "List of dired buffer sort modes.")

    (defvar dired-sort-current-mode ""
      "Current mode for sorting dired buffer.")

    ;; redefining from dired.el. Just cycle the options
    (defun dired-sort-toggle ()
      (cond
       ((equal dired-sort-current-mode "") (setq dired-sort-current-mode "S") (dired-sort-size))
       ((equal dired-sort-current-mode "S") (setq dired-sort-current-mode "X") (dired-sort-extension))
       ((equal dired-sort-current-mode "X") (setq dired-sort-current-mode "ct") (dired-sort-ctime))
       ((equal dired-sort-current-mode "ct") (setq dired-sort-current-mode "t") (dired-sort-utime))
       ((equal dired-sort-current-mode "t") (setq dired-sort-current-mode "") (dired-sort-name))))

    ;; redefining from dired.el. With double-prefix show a menu to chose the sorting from
    (defun dired-sort-toggle-or-edit (&optional arg)
      "Toggle sorting by date, and refresh the Dired buffer.

With a prefix argument \\[universal-argument], edit the current listing switches instead.

With a prefix argument \\[universal-argument] \\[universal-argument] prompt user with list of choices
to chose from."
      (interactive "P")
      (when dired-sort-inhibit
        (error "Cannot sort this dired buffer"))
      (cond
       ((equal arg '(4))
        (dired-sort-other
         (read-string "ls switches (must contain -l): " dired-actual-switches)))
       ((equal arg '(16))
        (let* ((sort-mode (completing-read "Sort by: "
                                           (mapcar 'car dired-sort-modes-list)
                                           nil
                                           t))
               (sort-switch (caddr (assoc sort-mode dired-sort-modes-list))))
          (setq dired-sort-current-mode sort-switch)
          (dired-sort-toggle)))
       (t (dired-sort-toggle))))

    (defun dired-sort-size ()
      "Dired sort by size."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "S")))

    (defun dired-sort-extension ()
      "Dired sort by extension."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "X")))

    (defun dired-sort-ctime ()
      "Dired sort by create time."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "ct")))

    (defun dired-sort-time ()
      "Dired sort by time."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "t")))

    (defun dired-sort-name ()
      "Dired sort by name."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "")))

    ;; redefined from dired.el to support new types of sorting
    (defun dired-sort-set-modeline ()
      (when (eq major-mode 'dired-mode)
        (setq mode-name
              (let (case-fold-search)
                (cond
                 ((string-match "ct" dired-actual-switches)
                  "Dired by ctime")
                 ((string-match "^-[^t]*t[^t]*$" dired-actual-switches)
                  "Dired by date")
                 ((string-match "^-[^X]*X[^X]*$" dired-actual-switches)
                  "Dired by ext")
                 ((string-match "^-[^S]*S[^S]*$" dired-actual-switches)
                  "Dired by size")
                 ((string-match "^-[^SXUt]*$" dired-actual-switches)
                  "Dired by name")
                 (t
                  (concat "Dired " dired-actual-switches)))))
        (force-mode-line-update)))

;;;_. Pretty colors
    (defmacro my-diredp-rainbow (symbol spec regexp &optional group)
      (setq group (or group 1))
      `(progn
         (defface ,symbol '((t ,spec)) "My diredp rainbow face" :group 'Dired-Plus)
         ,@(mapcar (lambda (m)
                     `(font-lock-add-keywords ',m '((,regexp ,group ',symbol))))
                   '(dired-mode))))

    (defmacro my-diredp-hilight-file (face-name color extensions)
      (let ((regexp (concat
                     "^[^!].[^d].*[0-9][ ]\\(.*\\."
                     (regexp-opt extensions)
                     "\\)$")))
        `(progn
           (defface ,face-name '((t (:foreground ,color))) "My diredp rainbow face" :group 'Dired-Plus)
           ,@(mapcar (lambda (m)
                       `(font-lock-add-keywords ',m '((,regexp 1 ',face-name))))
                     '(dired-mode)))))

    (my-diredp-hilight-file my-diredp-html-face "#4e9a06" ("htm" "html" "xhtml"))
    (my-diredp-hilight-file my-diredp-document-face "#fce94f" ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf"))
    (my-diredp-hilight-file my-diredp-xml-face "DarkGreen" ("xml" "xsd" "xsl" "xslt" "wsdl"))
    (my-diredp-hilight-file my-diredp-log-face "#c17d11" ("log"))
    (my-diredp-hilight-file my-diredp-compressed-face "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (my-diredp-hilight-file my-diredp-packaged-face "#e6a8df" ("deb" "rpm"))
    (my-diredp-hilight-file my-diredp-encrypted-face "#ce5c00" ("gpg" "pgp"))
    (my-diredp-hilight-file my-diredp-image-face "#ff4b4b" ("jpg" "png" "jpeg" "gif"))
    (my-diredp-hilight-file my-diredp-sourcefile-face "#fcaf3e" ("py" "c" "cc" "h" "java" "pl"))
    (my-diredp-hilight-file my-diredp-executable-face "#8cc4ff" ("exe" "msi"))

    (my-diredp-rainbow my-diredp-broken-link-face (:inherit dired-warning :italic t) "\\(^[!].l.*$\\)")

;;;_. Find dired hacked to work with windows
;;; re-eval `find-dired-filter' to replace \\ with /
    (use-package find-dired
      :defer t
      :config
      (progn
        (defun find-dired-filter (proc string)
          ;; Filter for \\[find-dired] processes.
          (let ((buf (process-buffer proc))
                (inhibit-read-only t))
            (if (buffer-name buf)
                (with-current-buffer buf
                  (save-excursion
                    (save-restriction
                      (widen)
                      (let ((buffer-read-only nil)
                            (beg (point-max))
                            (l-opt (and (consp find-ls-option)
                                        (string-match "l" (cdr find-ls-option))))
                            (ls-regexp (concat "^ +[^ \t\r\n]+\\( +[^ \t\r\n]+\\) +"
                                               "[^ \t\r\n]+ +[^ \t\r\n]+\\( +[0-9]+\\)")))
                        (goto-char beg)
                        (insert string)
                        (goto-char beg)
                        (or (looking-at "^")
                            (forward-line 1))
                        (while (looking-at "^")
                          (insert "  ")
                          (forward-line 1))
                        (goto-char (- beg 3))
                        (while (search-forward "\\\\" nil t)
                          (delete-region (point) (- (point) 2))
                          (insert "/"))
                        ;; Convert ` ./FILE' to ` FILE'
                        ;; This would lose if the current chunk of output
                        ;; starts or ends within the ` ./', so back up a bit:
                        (goto-char (- beg 3))  ; no error if < 0
                        (while (search-forward " ./" nil t)
                          (delete-region (point) (- (point) 2)))
                        ;; Pad the number of links and file size.  This is a
                        ;; quick and dirty way of getting the columns to line up
                        ;; most of the time, but it's not foolproof.
                        (when l-opt
                          (goto-char beg)
                          (goto-char (line-beginning-position))
                          (while (re-search-forward ls-regexp nil t)
                            (replace-match (format "%4s" (match-string 1))
                                           nil nil nil 1)
                            (replace-match (format "%9s" (match-string 2))
                                           nil nil nil 2)
                            (forward-line 1)))
                        ;; Find all the complete lines in the unprocessed
                        ;; output and process it to add text properties.
                        (goto-char (point-max))
                        (if (search-backward "\n" (process-mark proc) t)
                            (progn
                              (dired-insert-set-properties (process-mark proc)
                                                           (1+ (point)))
                              (move-marker (process-mark proc) (1+ (point)))))))))
              ;; The buffer has been killed.
              (delete-process proc))))))

    (defvar my-find-dired-ignore-dirs '(".hg" "backups" ".git")
      "Directories to ignore while searching.")

    (defvar my-find-dired-ignore-extensions '("aux" "toc" "elc")
      "File extensions to ignore while searching.")

    ;; see `find-dired' in find-dired.el
    (defun my-find-dired (dir pattern)
      (interactive
       (list
        (file-truename (ido-read-directory-name
                        "Find name (directory): "
                        default-directory default-directory t nil))
        (read-from-minibuffer "Find name (filename wildcard): ")))
      (when (not (featurep 'find-dired))
        (require 'find-dired))
      (let ((dired-buffers dired-buffers))
        ;; Expand DIR ("" means default-directory), and make sure it has a
        ;; trailing slash.
        (setq dir (file-name-as-directory (expand-file-name dir)))
        ;; Check that it's really a directory.
        (or (file-directory-p dir)
            (error "find-dired needs a directory: %s" dir))
        (switch-to-buffer (get-buffer-create "*Find*"))

        ;; See if there's still a `find' running, and offer to kill
        ;; it first, if it is.
        (let ((find (get-buffer-process (current-buffer))))
          (when find
            (if (or (not (eq (process-status find) 'run))
                    (yes-or-no-p "A `find' process is running; kill it? "))
                (condition-case nil
                    (progn
                      (interrupt-process find)
                      (sit-for 1)
                      (delete-process find))
                  (error nil))
              (error "Cannot have two processes in `%s' at once" (buffer-name)))))

        (widen)
        (kill-all-local-variables)
        (setq buffer-read-only nil)
        (erase-buffer)

        (setq default-directory dir)

        (let ((cmd (concat
                    "d:/progs/git/bin/bash -c \"cd "
                    dir "; find . \\( "
                    (mapconcat (lambda (d)
                                 (concat "-iname \\\"" d "\\\""))
                               my-find-dired-ignore-dirs " -o ")
                    " \\) -prune -o \\( -type f -iname \\\"*"
                    pattern "*\\\" -a -not \\( "
                    (mapconcat (lambda (ex)
                                 (concat "-iname \\\"*." ex "\\\""))
                               my-find-dired-ignore-extensions " -o ")
                    " \\) \\) -ls\" &")))
          (shell-command cmd (current-buffer)))

        (dired-mode dir (cdr find-ls-option))
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map (current-local-map))
          (define-key map "\C-c\C-k" 'kill-find)
          (use-local-map map))
        (make-local-variable 'dired-sort-inhibit)
        (setq dired-sort-inhibit t)
        (set (make-local-variable 'revert-buffer-function)
             `(lambda (ignore-auto noconfirm)
                (my-find-dired ,dir ,pattern)))
        ;; Set subdir-alist so that Tree Dired will work:
        (if (fboundp 'dired-simple-subdir-alist)
            ;; will work even with nested dired format (dired-nstd.el,v 1.15
            ;; and later)
            (dired-simple-subdir-alist)
          ;; else we have an ancient tree dired (or classic dired, where
          ;; this does no harm)
          (set (make-local-variable 'dired-subdir-alist)
               (list (cons default-directory (point-min-marker)))))
        (set (make-local-variable 'dired-subdir-switches) find-ls-subdir-switches)
        (setq buffer-read-only nil)
        ;; Subdir headlerline must come first because the first marker in
        ;; subdir-alist points there.
        (insert "  " dir ":\n")
        ;; Make second line a ``find'' line in analogy to the ``total'' or
        ;; ``wildcard'' line.
        (insert "  Results for: " pattern "\n")
        (setq buffer-read-only t)
        (let ((proc (get-buffer-process (current-buffer))))
          (set-process-filter proc (function find-dired-filter))
          (set-process-sentinel proc (function find-dired-sentinel))
          ;; Initialize the process marker; it is used by the filter.
          (move-marker (process-mark proc) 1 (current-buffer)))
        (setq mode-line-process '(":%s"))))

;;;_. Zip support

    (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))
    (add-to-list 'dired-compress-file-suffixes '("\\.gz\\'" "" "zcat"))
    (add-to-list 'dired-compress-file-suffixes '("\\.bz2\\'" "" "bzcatt"))
    (add-to-list 'dired-compress-file-suffixes '("\\.tar\\.bz2\\'" "" "untarbz2"))
    (add-to-list 'dired-compress-file-suffixes '("\\.tar\\.gz\\'" "" "untargz"))

    (defun my-dired-zip-files (zip-file)
      "Create an archive containing the marked files."
      (interactive
       (list (let ((files (dired-get-marked-files)))
               (read-from-minibuffer
                "Enter name of zip file: "
                (if (cadr files)
                    ;; more than one file selected, use directory name as default
                    (file-name-nondirectory
                     (substring default-directory 0 (1- (length default-directory))))
                  ;; otherwise use the current file name as defalt
                  (file-name-nondirectory (car files)))))))
      (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
        (shell-command
         (concat "zip -r "
                 (concat "\"" zip-file "\"")
                 " "
                 (mapconcat (lambda (obj) (format "\"%s\"" obj))
                            (mapcar
                             (lambda (filename)
                               (file-name-nondirectory filename))
                             (dired-get-marked-files)) " ")))
        (dired-unmark-all-marks)
        (revert-buffer)
        (goto-char 0)
        (when (search-forward zip-file nil t)
          (goto-char (match-beginning 0)))))

    (defun my-dired-unpack-files (where)
      "Unpack this archive into the target directory WHERE."
      (interactive
       (list (ido-read-directory-name
              "Enter target directory (where to unpack): "
              (dired-dwim-target-directory))))
      (let ((default-directory default-directory))
        (unless (file-directory-p where)
          (when (y-or-n-p (format "Directory %s does not exist. Create?" where))
            (make-directory where t)))
        (cd where)
        (dired-compress-file (car (dired-get-marked-files)))
        (revert-buffer)))
    ))

;;;_. Local var settings

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
