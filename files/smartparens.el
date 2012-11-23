(smartparens-global-mode t)

;; pending deletion. Replace active region with input. This is
;; virtually `delete-selection-mode' emulation.
(sp-turn-on-delete-selection-mode)

;;; add new pairs
(sp-add-pair "*" "*")

;;; global
(sp-add-ban-insert-pair-in-string "'")

;;; emacs-lisp-mode
(sp-add-local-ban-insert-pair "'" 'emacs-lisp-mode)
(sp-add-local-ban-insert-pair-in-code "`" 'emacs-lisp-mode)

;; you can also use the (sp-with) macro. It will automatically add the
;; mode to the end of each call. How cool is that!
(sp-with 'markdown-mode
         (sp-add-local-pair '("`" . "`"))
         (sp-add-local-ban-insert-pair "'")
         ;; this also disables '*' in all other modes
         (sp-add-local-allow-insert-pair "*"))
