(use-package smex
  :init
  (progn
    (bind-key "M-x" 'beautify-smex)
    (bind-key "M-X" 'smex-major-mode-commands)
    ;; This is your old M-x.
    (bind-key "C-c C-c M-x" 'execute-extended-command)

    (defun beautify-smex ()
      (interactive)
      (unwind-protect
          (progn
            (setq ido-decorations
                  '("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
            (smex))
        (setq ido-decorations
              '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))))
  :config
  (progn
    (defun smex-update-after-load (unused)
      (when (boundp 'smex-cache)
        (smex-update)))
    (add-hook 'after-load-functions 'smex-update-after-load)

    (defun smex-prepare-ido-bindings ()
      (define-key ido-completion-map (kbd "C-h f") 'smex-describe-function)
      (define-key ido-completion-map (kbd "C-h w") 'smex-where-is)
      (define-key ido-completion-map (kbd "M-.") 'smex-find-function)
      (define-key ido-completion-map (kbd "C-a") 'move-beginning-of-line)
      (define-key ido-completion-map (kbd "=") "-"))))
