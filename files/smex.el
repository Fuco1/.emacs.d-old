(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

(defun beautify-smex ()
  (interactive)
  (unwind-protect (progn (setq ido-decorations '( "{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
                         (smex))
    (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))))

(bind-key "M-x" 'beautify-smex)
(bind-key "M-X" 'smex-major-mode-commands)
;; This is your old M-x.
(bind-key "C-c C-c M-x" 'execute-extended-command)
