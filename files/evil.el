(define-key evil-normal-state-map [?\e] 'evil-force-normal-state)
(define-key evil-visual-state-map [?\e] 'evil-exit-visual-state)
(define-key evil-insert-state-map [?\e] 'evil-normal-state)
(define-key evil-replace-state-map [?\e] 'evil-normal-state)
(define-key evil-ex-completion-map [?\e] 'abort-recursive-edit)

(setq evil-normal-state-tag   (propertize "N" 'face '((:background "#73d216" :foreground "black")))
      evil-emacs-state-tag    (propertize "E" 'face '((:background "#f57900" :foreground "black")))
      evil-insert-state-tag   (propertize "I" 'face '((:background "#cc0000")))
      evil-motion-state-tag   (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag   (propertize "V" 'face '((:background "#eeeeec" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

(setq evil-normal-state-cursor '("#73d216" t)
      evil-visual-state-cursor '("#eeeeec" t))

(evil-define-operator sp-evil-move-end (beg end type)
  "Move to the end of the next motion or text object."
  (interactive "<R>")
  (message "beg: %s end: %s type: %s" beg end type)
  (goto-char end))

(evil-define-operator sp-evil-move-beginning (beg end type)
  "Move to the beginning of the next motion or text object."
  (interactive "<R>")
  (goto-char beg))

(evil-define-text-object sp-evil-a-sexp (count &optional beg end type)
  "Smartparens sexp object."
  (save-excursion
    (let ((obj (sp--next-thing-selection count)))
      (sp-get obj
        (message "a-sexp %s %s" :beg-prf :end)
        (list :beg-prf :end)))))

(evil-define-text-object sp-evil-a-paren (count &optional beg end type)
  "Smartparens paren object.  This is the enclosing sexp."
  (let ((obj (sp-get-enclosing-sexp count)))
    (sp-get obj (list :beg-prf :end))))

(defvar sp-prefix-beg-of-obj nil)

(evil-define-text-object sp-evil-inner-paren (count &optional beg end type)
  "Smartparens inner paren object.  This is insides of the
enclosing sexp."
  (let ((obj (if sp-prefix-beg-of-obj
                 (progn (message "inner, foo") (sp--next-thing-selection '(4)))
               (message "inner, bar")
               (sp-get-enclosing-sexp count))))
    (sp-get obj (message "inner paren, %s %s" :beg-in :end-in))
    (sp-get obj (list :beg-in :end-in))))

(define-key evil-inner-text-objects-map "[" 'sp-evil-inner-paren)
(define-key evil-inner-text-objects-map "]" 'sp-evil-inner-paren)

(evil-define-text-object sp-evil-end-of-obj (count beg end)
  "Modify the next object to be an end movement."
  (interactive "<c><r>")
  (message "count: %s beg: %s end: %s" count beg end)
  (list (point) end))

(evil-define-text-object sp-evil-beg-of-obj (count beg end)
  "Modify the next object to be a beg movement."
  (interactive "<c>" (let ((sp-prefix-beg-of-obj t)) (evil-operator-range)))
  (message "beg obj, count: %s beg: %s end: %s" count beg end)
  (list beg end))

(evil-define-text-object sp-evil-pair-obj (count beg end)
  "Modify the next object to be a pair movement."
  (interactive "<c>" (let ((sp-prefix-pair-object t)) (evil-operator-range)))
  (message "pair obj, count: %s beg: %s end: %s" count beg end)
  (list beg end))

(evil-define-text-object sp-evil-tag-obj (count beg end)
  "Modify the next object to be a tag movement."
  (interactive "<c>" (let ((sp-prefix-tag-object t)) (evil-operator-range)))
  (message "tag obj, count: %s beg: %s end: %s" count beg end)
  (list beg end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil bindings, smartparens related

(define-key evil-outer-text-objects-map "e" 'sp-evil-a-sexp)
(define-key evil-outer-text-objects-map "[" 'sp-evil-a-paren)
(define-key evil-outer-text-objects-map "]" 'sp-evil-a-paren)

;; [[ etc. navigated defuns, [( etc. navigated (backward)-up-list
(define-key evil-motion-state-map "[" 'sp-evil-beg-of-obj)
(define-key evil-motion-state-map "]" 'sp-evil-end-of-obj)
(define-key evil-motion-state-map "/" 'sp-evil-pair-obj)
(define-key evil-motion-state-map "?" 'sp-evil-tag-obj)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom evil bindings

(define-key evil-normal-state-map "M" 'evil-set-marker)

;; my own evil movements. Some default bindings were moved here.
(defvar my-evil-move-map (make-sparse-keymap))
(define-prefix-command 'my-evil-move-map)
(define-key evil-normal-state-map "m" my-evil-move-map)
(define-key my-evil-move-map "]]" 'evil-forward-section-begin)
(define-key my-evil-move-map "][" 'evil-forward-section-end)
(define-key my-evil-move-map "[[" 'evil-backward-section-begin)
(define-key my-evil-move-map "[]" 'evil-backward-section-end)
(define-key my-evil-move-map "d" 'sp-down-sexp)
(define-key my-evil-move-map "a" 'sp-backward-down-sexp)

(define-key evil-normal-state-map (kbd "C-.") nil)
