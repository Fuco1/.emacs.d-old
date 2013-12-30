;; see commentary in dired-defs.el
(use-package dired
  :mode ("\\.wdired\\'" . my-virtual-dired-mode)
  :bind (("C-x d"  . my-dired-files)
         ("M-<f2>" . my-find-dired)
         ("C-x C-j" . dired-jump))
  :init
  (progn
    (defun my-virtual-dired-mode ()
      (save-excursion
        (goto-char (point-min))
        (back-to-indentation)
        (let ((ddir (thing-at-point 'filename)))
          (virtual-dired (substring ddir 0 (1- (length ddir)))))
        (dired-virtual-revert)))

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
