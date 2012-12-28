(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;; key binds
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

;;; add new pairs
(sp-add-pair "*" "*")
(sp-add-pair "$" "$")
(sp-add-pair "<" ">") ;; in html only!

;;; global
(sp-add-ban-insert-pair-in-string "'")

;; you can also use the `sp-with-tag' macro. It will automatically add
;; the tag to each function. Use this only with functions where the
;; first argument is the opening pair! Here, we want to disable ' pair
;; in a bunch of text modes
(sp-with-tag "'"
             (sp-add-local-ban-insert-pair 'markdown-mode)
             (sp-add-local-ban-insert-pair 'tex-mode)
             (sp-add-local-ban-insert-pair 'latex-mode)
             (sp-add-local-ban-insert-pair 'text-mode)
             (sp-add-local-ban-insert-pair 'log-edit-mode)
             (sp-add-local-ban-insert-pair 'org-mode))

;; now, we could've also done just this:
;; (sp-add-local-ban-insert-pair "'"
;;                               '(markdown-mode
;;                                 tex-mode
;;                                 latex-mode
;;                                 text-mode
;;                                 log-edit-mode))
;; but I wanted to show you how to use the sp-with-tag macro :)

;;; emacs-lisp-mode
(sp-add-local-ban-insert-pair "'" 'emacs-lisp-mode)
(sp-add-local-ban-insert-pair "'" 'inferior-emacs-lisp-mode)
(sp-add-local-ban-insert-pair-in-code "`" 'emacs-lisp-mode)
(sp-add-local-ban-insert-pair-in-code "`" 'inferior-emacs-lisp-mode)

;;; markdown-mode
;; you can also use the `sp-with' macro. It will automatically add the
;; mode to the end of each call. How cool is that!
(sp-with '(markdown-mode rst-mode)
         (sp-add-local-pair "`" "`")
         ;; this also disables '*' in all other modes
         (sp-add-local-allow-insert-pair "*")
         (sp-add-tag-pair "2" "**" "**" nil))

;;; tex-mode latex-mode
(sp-with '(tex-mode latex-mode) ;; yes, this works with lists too!
         (sp-add-local-allow-insert-pair "$"))

;;; python-mode
(sp-with 'python-mode
         (sp-add-local-ban-insert-pair "`"))

;;; html-mode
(sp-with '(html-mode sgml-mode)
         (sp-add-local-allow-insert-pair "<"))
