;; see commentary in dired-defs.el
(use-package dired
  :bind (("C-x d"  . my-dired-files)
         ("M-<f2>" . my-find-dired)
         ("C-x C-j" . dired-jump))
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
