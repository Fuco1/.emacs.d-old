;; loads dired, dired-aux, dired-x
(require 'dired+)
(use-package dired-details
  :commands dired-details-toggle)

;;;_. Key bindings & hooks
(defun my-image-dired-thumbnail-mode-init ()
  (bind-key "b" 'image-dired-backward-image image-dired-thumbnail-mode-map)
  (bind-key "f" 'image-dired-forward-image image-dired-thumbnail-mode-map)
  (bind-key "n" 'image-dired-next-line image-dired-thumbnail-mode-map)
  (bind-key "p" 'image-dired-previous-line image-dired-thumbnail-mode-map)
  (bind-key "q" 'kill-this-buffer image-dired-thumbnail-mode-map))
(add-hook 'image-dired-thumbnail-mode-hook 'my-image-dired-thumbnail-mode-init)

(defun my-image-dired-display-image-init ()
  (bind-key "q" 'kill-this-buffer image-dired-display-image-mode-map)
  (bind-key "SPC" 'my-image-dired-display-next image-dired-display-image-mode-map)
  (bind-key "<backspace>" 'my-image-dired-display-previous image-dired-display-image-mode-map)

  (bind-key "<wheel-down>" 'my-image-dired-display-next image-dired-display-image-mode-map)
  (bind-key "<wheel-up>" 'my-image-dired-display-previous image-dired-display-image-mode-map)

  (bind-key "m" 'my-image-dired-mark-image-in-dired image-dired-display-image-mode-map)
  (bind-key "u" 'my-image-dired-unmark-image-in-dired image-dired-display-image-mode-map)


  (bind-key "RET" 'my-image-dired-display-open image-dired-display-image-mode-map)
  (bind-key "M-RET" 'my-image-dired-display-external image-dired-display-image-mode-map))
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

  (defvar slash-dired-prefix-map)
  (define-prefix-command 'slash-dired-prefix-map)
  (bind-key "/" 'slash-dired-prefix-map dired-mode-map)
  (bind-key "/ r" 'my-dired-filter-by-regexp dired-mode-map)
  (bind-key "/ e" 'my-dired-filter-by-ext dired-mode-map)
  (bind-key "/ /" 'my-dired-filter-by-name dired-mode-map)

  (bind-key "(" 'dired-details-toggle dired-mode-map)

  (bind-key "M-<f5>" 'dired-arc-pack-files dired-mode-map)
  (bind-key "M-<f6>" 'dired-arc-unpack-file dired-mode-map)
  (bind-key "l" 'dired-arc-list-archive dired-mode-map)

  (dired-omit-mode t)
  (visual-line-mode -1)
  (toggle-truncate-lines 1))
(add-hook 'dired-mode-hook 'my-dired-init)

(defun my-dired-filter-by-regexp (regexp)
  (interactive "sRegexp: ")
  (let ((cbuffer (current-buffer)))
    (dired-mark-files-regexp regexp)
    (call-interactively 'diredp-marked)
    (with-current-buffer cbuffer
      (dired-unmark-all-marks))))

(defun my-dired-filter-by-ext (ext)
  (interactive "sExtension: ")
  (let ((cbuffer (current-buffer)))
    (dired-mark-files-regexp (concat "\\." ext "\\'"))
    (call-interactively 'diredp-marked)
    (with-current-buffer cbuffer
      (dired-unmark-all-marks))))

(defun my-dired-filter-by-name (name)
  (interactive "sName: ")
  (let ((cbuffer (current-buffer)))
    (dired-mark-files-regexp (regexp-quote name))
    (call-interactively 'diredp-marked)
    (with-current-buffer cbuffer
      (dired-unmark-all-marks))))

;; TODO: pridat C-b z totalcmd

(defun my-image-dired--with-image-in-dired (operation)
  "OPERATION is a function of two arguments, the file we operate
on and associated dired buffer."
  (let* ((old-buf (current-buffer))
         (file (image-dired-original-file-name))
         (dired-name (progn
                       (string-match ".*/\\(.*\\)/.*" file)
                       (match-string 1 file)))
         (dired-buf (get-buffer dired-name)))
    (if (not dired-buf)
        (error "No associated dired buffer found.")
      (set-buffer dired-buf)
      (funcall operation file dired-buf)
      (set-buffer old-buf))))

(defun my-image-dired--skip-non-image (arg)
  "Skip non-image files in direction ARG"
  (while (and (or (file-directory-p (car (dired-get-marked-files)))
                  (not (eq (get-char-property (point) 'face) 'my-diredp-image-face)))
              (not (eobp))
              (not (bobp)))
    (dired-next-line arg)
    (set-window-point (get-buffer-window dired-buf) (point))))

(defun my-image-dired-display-next (arg)
  (interactive "p")
  (my-image-dired--with-image-in-dired
   (lambda (file dired-buf)
     (dired-goto-file file)
     (dired-next-line arg)
     (ignore-errors (my-image-dired--skip-non-image arg))
     (let ((next-img (ignore-errors (dired-get-marked-files))))
       (unless next-img
         (if (> arg 0)
             (progn
               (goto-char (point-min))
               (dired-next-line (if dired-omit-mode 2 4)))
           (goto-char (point-max))
           (dired-previous-line 1))
         (my-image-dired--skip-non-image arg))
       (image-dired-dired-display-image)
       (set-window-point (get-buffer-window dired-buf) (point))))))

(defun my-image-dired-display-previous ()
  (interactive)
  (my-image-dired-display-next -1))

(defun my-image-dired-goto-image-in-dired ()
  (interactive)
  (my-image-dired--with-image-in-dired
   (lambda (file dired-buf)
     (dired-goto-file file)
     (set-window-point (get-buffer-window dired-buf) (point)))))

(defun my-image-dired-mark-image-in-dired ()
  (interactive)
  (my-image-dired--with-image-in-dired
   (lambda (file dired-buf)
     (dired-goto-file file)
     (dired-mark 1)))
  (my-image-dired-display-next 1))

(defun my-image-dired-unmark-image-in-dired ()
  (interactive)
  (my-image-dired--with-image-in-dired
   (lambda (file dired-buf)
     (dired-goto-file file)
     (dired-unmark 1))))

(defun my-image-dired-display-open ()
  (interactive)
  (find-file (image-dired-original-file-name)))

(defun my-image-dired-display-external ()
  (interactive)
  (w32-browser (image-dired-original-file-name)))

(defadvice select-window (after image-dired-resize-image)
  (when (eq major-mode 'image-dired-display-image-mode)
    (image-dired-display-current-image-sized)))

(defadvice other-window (around image-dired-resize-image)
  (let ((buffer (current-buffer)))
    ad-do-it
    (with-current-buffer buffer
      (when (eq major-mode 'image-dired-display-image-mode)
        (image-dired-display-current-image-sized)))
    (when (eq major-mode 'image-dired-display-image-mode)
      (image-dired-display-current-image-sized))))


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
   ((equal dired-sort-current-mode "ct") (setq dired-sort-current-mode "t") (dired-sort-time))
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
                    (while (search-forward "\\" nil t)
                      (delete-region (point) (- (point) 1))
                      (insert "/"))
                    (while (search-forward "\\.\\" nil t)
                      (delete-region (point) (- (point) 2)))
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
                " \\) \\) "
                (car find-ls-option)
                " \" &")))
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

(defvar dired-arc-unpack-list
  '(("\\.zip\\'" (:program "7z" :switches "x -y"))
    ("\\.rar\\'" (:program "7z" :switches "x -y"))
    ("\\.tar\\'" (:program "7z" :switches "x -y"))
    ("\\.tar\\.gz\\'"
     (:program "bsdtar" :switches "zxvf")
     (:program "gzip" :switches "-df" :name-after ".tar")
     (:program "gzip" :switches "-dkf" :name-after ".tar" :comment "Keep original file"))
    ("\\.gz\\'"
     (:program "7z" :switches "x -y")
     (:program "gzip" :switches "-df" :name-after ""))
    ("\\.tar.\\bz2\\'"
     (:program "bsdtar" :switches "jxvf")
     (:program "bunzip2" :switches "-f" :name-after ".tar")
     (:program "bunzip2" :switches "-fk" :name-after ".tar" :comment "Keep original file"))
    ("\\.bz2\\'"
     (:program "7z" :switches "x -y")
     (:program "bunzip2" :switches "-f" :name-after ""))
    ("\\.tgz\\'"
     (:program "bsdtar" :switches "zxvf")
     (:program "gzip" :switches "-df" :name-after ".tar")
     (:program "gzip" :switches "-dkf" :name-after ".tar" :comment "Keep original file"))
    ("\\.tbz\\'"
     (:program "bsdtar" :switches "jxvf")
     (:program "bunzip2" :switches "-f" :name-after ".tar")
     (:program "bunzip2" :switches "-fk" :name-after ".tar" :comment "Keep original file")))
  "List assigning file extensions to methods to unpack them.
The file name is also appended after the program and switches
automatically.

Each method is a plist with these possible keys:
  :program - a program to run
  :switches - switches to pass to the program
  :name-after - for gz/bz2 etc. this will specify how the matched
    extension will be replaced after the extraction.  Useful to
    ask for confirmation if the extraction will be run as async
    process.

If more than one sensible methods exist, they are listed one
after another using the same format.")

(defun dired-arc-pack-files (zip-file)
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

(defun dired-arc-unpack-file (where)
  "Unpack this archive into the target directory WHERE.
With \\[universal-argument] present user with list of possible methods to unpack the file."
  (interactive
   (list (ido-read-directory-name
          "Enter target directory (where to unpack): "
          (dired-dwim-target-directory))))
  (let ((default-directory default-directory))
    (unless (file-directory-p where)
      (when (y-or-n-p (format "Directory %s does not exist. Create?" where))
        (make-directory where t)))
    (cd where)
    (let* ((file (car (dired-get-marked-files)))
           (method (cdr (--first (string-match-p (car it) file) dired-arc-unpack-list)))
           (cmd (and method
                     (progn
                       (cond
                        ((equal '(4) current-prefix-arg)
                         (let* ((choices (mapcar (lambda (m)
                                                   (cons (concat
                                                          (plist-get m :program)
                                                          " " (plist-get m :switches)
                                                          (when (plist-get m :comment)
                                                            (concat " (" (plist-get m :comment) ")"))) m))
                                                 method))
                                (r (completing-read
                                    "Unpacking method: "
                                    choices
                                    nil t nil nil (caar choices))))
                           (setq method (cdr (assoc r choices)))))
                        (t (setq method (car method))))
                       (concat
                        (plist-get method :program)
                        " " (plist-get method :switches) " \""
                        file "\"")))))
      (shell-command cmd))
    (revert-buffer)))

(defun dired-arc-list-archive ()
  (interactive)
  (let ((buffer (get-buffer-create "*Archive listing*")))
    (with-current-buffer buffer
      (widen)
      (kill-all-local-variables)
      (setq buffer-read-only nil)
      (erase-buffer))
    (shell-command (concat "7z l \"" (car (dired-get-marked-files)) "\"")
                   buffer)
    (with-current-buffer buffer
      (goto-char 1)
      (unless (search-forward "Can not open file as archive" 300 t)
        (goto-char 1)
        (delete-region (point) (progn (forward-line 6) (point)))
        (search-forward "-----------")
        (beginning-of-line 0)
        (delete-region (point) (progn (forward-line 2) (point)))
        (insert "  ---------- ----------- ----------- ------------------------\n")
        (let (date size total)
          (while (not (looking-at "[ ]*-----------"))
            ;; copy the date here
            (delete-char 5)
            (let ((p (point)))
              (forward-char 11)
              (setq date (buffer-substring-no-properties p (point)))
              (delete-char -11))
            (delete-char 4)
            (if (looking-at "D")
                (insert "  drwxrwxrwx")
              (insert "  -rwxrwxrwx"))
            (delete-char 6)
            (let ((p (point)))
              (forward-char 13)
              (setq size (buffer-substring-no-properties p (point)))
              (delete-char -13)
              (insert (ls-lisp-format-file-size (string-to-int size) t) " "))
            (delete-char 13)
            (insert date)
            (forward-line))
          (delete-region (point) (progn (forward-line 1) (point)))
          (insert "  ---------- ----------- ----------- ------------------------\n")
          (forward-char 25)
          (let ((p (point)))
            (forward-char 13)
            (setq total (buffer-substring-no-properties p (point)))
            (delete-char -13))
          (delete-char -13)
          (insert (ls-lisp-format-file-size (string-to-int total) t) " ")
          (delete-char 13)
          (insert "          "))
        (goto-char 1)
        (replace-string "\\" "/")
        (dired-mode "/")))))

;;;_. Local var settings

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End: