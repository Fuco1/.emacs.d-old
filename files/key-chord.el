(key-chord-mode 1)

(key-chord-define-global "df" 'iy-go-to-char)

;; (defun add-tex-key-chords ()
;;   (key-chord-define tex-mode-map "$$" "$$\C-b"))
;; (add-hook 'tex-mode-hook 'add-tex-key-chords)

(defun add-programming-chords ()
  (key-chord-define-local ";;" "\C-e;")
  (key-chord-define-local ",," "\C-e,"))
(dolist (hook '(c++-mode-hook
                c-mode-hook
                js-mode-hook
                java-mode-hook))
  (add-hook hook 'add-programming-chords))
