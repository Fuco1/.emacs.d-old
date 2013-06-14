;;;;;;;;;
;; global
(require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap [remap delete-char] 'sp-delete-char)
(define-key sp-keymap [remap backward-delete-char-untabify] 'sp-backward-delete-char)
(define-key sp-keymap [remap backward-delete-char] 'sp-backward-delete-char)
(define-key sp-keymap [remap delete-backward-char] 'sp-backward-delete-char)
(define-key sp-keymap [remap kill-word] 'sp-kill-word)
(define-key sp-keymap [remap backward-kill-word] 'sp-backward-kill-word)

(bind-key "H-t" 'sp-prefix-tag-object sp-keymap)
(bind-key "H-p" 'sp-prefix-pair-object sp-keymap)
(bind-key "H-y" 'sp-prefix-symbol-object sp-keymap)
(bind-key "H-h" 'sp-highlight-current-sexp sp-keymap)
(bind-key "H-e" 'sp-prefix-save-excursion sp-keymap)
(bind-key "H-s c" 'sp-convolute-sexp sp-keymap)
(bind-key "H-s a" 'sp-absorb-sexp sp-keymap)
(bind-key "H-s e" 'sp-emit-sexp sp-keymap)
(bind-key "H-s p" 'sp-add-to-previous-sexp sp-keymap)
(bind-key "H-s n" 'sp-add-to-next-sexp sp-keymap)
(bind-key "H-s j" 'sp-join-sexp sp-keymap)
(bind-key "H-s s" 'sp-split-sexp sp-keymap)
(bind-key "H-s r" 'sp-rewrap-sexp sp-keymap)
(defvar hyp-s-x-map)
(define-prefix-command 'hyp-s-x-map)
(bind-key "H-s x" hyp-s-x-map sp-keymap)
(bind-key "H-s x x" 'sp-extract-before-sexp sp-keymap)
(bind-key "H-s x a" 'sp-extract-after-sexp sp-keymap)
(bind-key "H-s x s" 'sp-swap-enclosing-sexp sp-keymap)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
(bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-pair "_" "_" :bind "C-_")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; lisp modes
(sp-with-modes (append '(inferior-emacs-lisp-mode) sp--lisp-modes)
  (sp-local-pair "(" nil :bind "C-("))

;;; haskell mode
(sp-local-pair 'haskell-mode "'" nil :unless '(my-after-symbol-p))

(defun my-after-symbol-p (_id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\\sw\\|\\s_\\|\\s'"))))

(sp-local-pair 'emacs-lisp-mode "(" nil :post-handlers '(my-add-space-after-sexp-insertion))

(defun my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (length (plist-get (sp-get-pair id) :close)))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))
