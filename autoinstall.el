;; autoinstall packages
(require 'cl)

(defvar prelude-packages
  '(
    ack-and-a-half
    bookmark+
    browse-kill-ring
    dash
    diminish
    dired+
    dired-details
    expand-region
    haskell-mode
    ido-ubiquitous
    keyfreq
    magit
    markdown-mode
    markdown-mode+
    multi-web-mode
    multiple-cursors
    parenface
    php-mode
    projectile
    rainbow-mode
    s
    smex
    undo-tree
    w32-browser
    )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'autoinstall)
