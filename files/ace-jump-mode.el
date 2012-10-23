;;; Quick navigation jumping by word, char, or line

; Make all jumps case-insensitive
(setq ace-jump-mode-case-sensitive-search nil)

;; you can select the key you prefer to (hyperfind!)
(define-key global-map (kbd "H-f") 'ace-jump-mode)
