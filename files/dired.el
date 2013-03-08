;;;_. Commentary & basic stuff
;; diredp modified stuff:
;; comment out:
;; in `diredp-font-lock-keywords-1'
;;   line about compressed files
;;   line about ignored files "\\|\\.\\(g?z\\|Z\\)[*]?\\)\\)$") ; Compressed.
;;                            "\\)\\)$")
;;      '("[^ .]\\.\\([^. /]+\\)$" 1 diredp-file-suffix) ; Suffix
;; to:  '("[^ .\\/]\\.\\([^. /]+\\)$" 1 diredp-file-suffix) ; Suffix

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
    (load "files/dired-defs")))

;;;_. Local var settings

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
