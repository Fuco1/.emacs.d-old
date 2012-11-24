(smartparens-global-mode t)

;; pending deletion. Replace active region with input. This is
;; virtually `delete-selection-mode' emulation.
(sp-turn-on-delete-selection-mode)

;;; add new pairs
(sp-add-pair "*" "*")
(sp-add-pair "$" "$")

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
             (sp-add-local-ban-insert-pair 'log-edit-mode))

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
(sp-with 'markdown-mode
         (sp-add-local-pair "`" "`")
         ;; this also disables '*' in all other modes
         (sp-add-local-allow-insert-pair "*"))

;;; tex-mode latex-mode
(sp-with '(tex-mode latex-mode) ;; yes, this works with lists too!
         (sp-add-local-allow-insert-pair "$"))
