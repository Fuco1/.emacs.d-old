;;; Global key bindigns

;; setting the PC keyboard's various keys to
;; Super or Hyper, or emacs running on Windows.
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

;;; stupid terminal key sequence remapping
(define-key key-translation-map [return] [?\r])
(define-key key-translation-map [?\C-\m] [(control m-key)])
(define-key function-key-map [return] nil)
(define-key function-key-map [?\r] nil)

(define-key key-translation-map [tab] [?\t])
(define-key key-translation-map [?\C-\i] [(control i-key)])
(define-key function-key-map [tab] nil)
(define-key function-key-map [?\t] nil)

(define-key key-translation-map [escape] [?\e])
(define-key input-decode-map [?\C-\[] [(control left_bracket)])
(define-key function-key-map [escape] nil)
(define-key function-key-map [?\e] nil)

;; Url & Browsing
(bind-key "C-c C-w" 'browse-url-at-point)
(bind-key "C-c w" 'browse-url)

;; Find stuff
(bind-key "<f2>" 'occur)
(bind-key "n" 'occur-next occur-mode-map)
(bind-key "p" 'occur-prev occur-mode-map)
(bind-key "o" 'occur-mode-display-occurrence occur-mode-map)

;; is this safe binding?
(bind-key "C-'" 'repeat)

;; refresh-like
(bind-key "<f5>" '(lambda () (interactive) (load-file (buffer-file-name))))
(bind-key "A-u" 'universal-argument)
(bind-keys :prefix "A-x"
           :prefix-map alt-x-prefix-map
           :prefix-docstring "A-x prefix map"
           ("A-m" . my-find-file-same-mode)
           ("A-x" . my-find-file-same-ext)
           ("A-s" . my-find-file-sudo))

;; Indenting and alignment
(bind-key "<f8>" 'align-regexp)
(bind-key "C-<f8>" 'indent-buffer)
(bind-key "C-<tab>" 'indent-defun)

(defvar f1-prefix-map)
(define-prefix-command 'f1-prefix-map)
(bind-key "<f1>" 'f1-prefix-map)
(bind-key "<f1> <f2>" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(bind-key "<f1> <f3>" 'view-echo-area-messages)
(bind-key "<f1> <f4>" 'ffap)
(bind-key "<f1> <f5>" (lambda () (interactive) (let ((default-directory "~")) (ido-find-file))))
(bind-key "<f1> <f12>" 'my-switch-to-scratch)

;; ibuffer > list-buffers
(bind-key "C-x C-b" 'ibuffer)
(bind-key "C-<m-key>" 'ido-switch-buffer)
(bind-key "C-x C-k" 'kill-buffer-and-window)

;; buffer cleanup
(bind-key "C-c u" 'cleanup-buffer)

;; buffer switching using cycbuf
(bind-key "<right>" 'cycbuf-switch-to-next-buffer)
(bind-key "<left>" 'cycbuf-switch-to-previous-buffer)
(bind-key "<down>" 'cycbuf-discard-status-window)

;; Window navigation
(windmove-default-keybindings 'meta)

;; Easier buffer killing
(bind-key "M-k" 'my-kill-this-buffer)
(bind-key "M-K" (lambda () (interactive) (kill-buffer (window-buffer (next-window)))))

;; imenu
(bind-key "M-," 'find-function)

;; sexp settings
(bind-key "C-x e" 'eval-and-replace)
(bind-key "C-;" 'eval-expression)

;; minibuffer history
(bind-key "C-p" 'previous-history-element minibuffer-local-map)
(bind-key "C-n" 'next-history-element minibuffer-local-map)

(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c =" 'count-matches)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation

;; remove arrows, evil!
(mapc 'global-unset-key [[up] [down] [left] [right]])

;; # move by (forw/back)
;; character
(bind-key "C-f" 'forward-char)
(bind-key "C-b" 'backward-char)
;; word
(bind-key "M-f" 'forward-word)
(bind-key "M-b" 'backward-word)
;; line
(bind-key "C-n" 'next-line)
(bind-key "C-p" 'previous-line)
;; sentence
(bind-key "M-e" 'forward-sentence)
(bind-key "M-a" 'backward-sentence)
;; paragraph
(bind-key "M-[" 'backward-paragraph)
(bind-key "M-]" 'forward-paragraph)
(bind-key "M-{" 'backward-paragraph-select)
(bind-key "M-}" 'forward-paragraph-select)
;; screen
(bind-key "C-v" 'scroll-up-command)
(bind-key "M-v" 'scroll-down-command)
;; sexp
(bind-key "C-M-f" 'forward-sexp)
(bind-key "C-M-b" 'backward-sexp)
;; list
(bind-key "C-M-n" 'forward-list)
(bind-key "C-M-p" 'backward-list)
(bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)))
(bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)))

(bind-key "C-c q s" 'query-replace)
(bind-key "C-c q r" 'query-replace-regexp)

;; # move to (beg/end)
;; line
;; swap C-a and M-m, back-to-indentation is much more common
(bind-key "M-m" 'move-beginning-of-line)
(bind-key "C-a" 'my-back-to-indentation-or-beginning)
(bind-key "C-e" 'my-end-of-code-or-line)
;; buffer
(unbind-key "<home>")
(unbind-key "<end>")
(bind-key "M-<" 'beginning-of-buffer)
(bind-key "C-M-," 'beginning-of-buffer)
(bind-key "M->" 'end-of-buffer)
(bind-key "C-M-." 'end-of-buffer)

;; defun
(bind-key "M-p" 'beginning-of-defun)
(bind-key "M-n" 'end-of-defun)
;; active region
(bind-key "M-g a" 'beginning-of-region)
(bind-key "M-g e" 'end-of-region)

;; # move into
(bind-key "C-M-d" 'down-list)

(bind-key "C-c <up>" 'copy-previous-line)
(bind-key "C-c <down>" 'copy-next-line)
(bind-key "M-<up>" 'move-line-up)
(bind-key "M-<down>" 'move-line-down)

;; opening new lines. C-o can be called from any point on the line,
;; ret from the end only
(bind-key "RET" 'my-newline)
(bind-key "C-o" 'my-open-line)
(bind-key "C-S-o" 'forward-line-and-indent)
(bind-key "M-j"
          (lambda ()
            (interactive)
            (join-line -1)))

;; deleting stuff
(bind-key "C-<i-key>" 'backward-kill-word)
(bind-key "C-<backspace>" 'my-kill-whitespace)
(bind-key "C-c d" 'my-kill-entire-line)

;; up/downcase
(bind-key "M-l" 'my-smart-downcase-word)
(bind-key "M-u" 'my-smart-upcase-word)
(bind-key "M-c" 'my-capitalize-word)

;;;;; multiple cursors
(bind-key "C-c C-S-c" 'mc/edit-lines)
(bind-key "s-." 'mc/mark-next-like-this)
(bind-key "s-," 'mc/mark-previous-like-this)
(bind-key "s-\\" 'mc/mark-more-like-this-extended)
(bind-key "s-/" 'mc/mark-all-like-this-dwim)
(bind-key "H-SPC" 'set-rectangular-region-anchor)

(add-hook 'html-mode-hook (lambda () (bind-key "C-c <deletechar>" 'sgml-delete-tag html-mode-map)))

;; mark commands
(bind-key "C-`" 'push-mark-no-activate)
(bind-key "M-`" 'jump-to-mark)
(global-set-key [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; customize
(defvar customize-map)
(define-prefix-command 'customize-map)
(bind-key "C-c c" 'customize-map)
(bind-key "C-c c v" 'customize-variable)
(bind-key "C-c c f" 'customize-face)
(bind-key "C-c c g" 'customize-group)

;; calc settings
(bind-key "<pause>" 'calc-dispatch)
(bind-key "<home>" 'calc-same-interface)
(fset 'calc-one-minus [?1 return ?- ?n])
(fset 'calc-standard-normal [?0 return tab ?1 return tab ?k ?N ?1 return ?- ?n])
(eval-after-load "calc"
  '(progn
     (bind-key "C-. m" 'calc-one-minus calc-mode-map)
     (bind-key "C-. n" 'calc-standard-normal calc-mode-map)
     (bind-key "C-<tab>" 'calc-roll-up calc-mode-map)))

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)
(bind-key "C-h e" 'lisp-find-map)
(bind-key "C-h e b" 'free-keys)
(bind-key "C-h e d" 'info-lookup-symbol)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e F" 'find-face-definition)
(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)
(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)
(bind-key "C-h D" 'describe-personal-keybindings)

(defvar ctl-dot-prefix-map)
(define-prefix-command 'ctl-dot-prefix-map)
(bind-key "C-." 'ctl-dot-prefix-map)
(bind-key "C-. p f" 'elp-instrument-function)
(bind-key "C-. p r" 'elp-results)
(bind-key "C-. p R" 'elp-restore-all)

(bind-key "C-. u" 'unfill-paragraph)
(bind-key "C-. c" 'create-scratch-buffer)
(bind-key "C-. k" 'browse-kill-ring)

(bind-key "C-. m s" 'kmacro-set-counter)
(bind-key "C-. m a" 'kmacro-add-counter)
(bind-key "C-. m f" 'kmacro-set-format)

(bind-key "C-. e" 'eval-region emacs-lisp-mode-map)

(bind-key "C-. z" 'my-insert-no-move)

(bind-key "C-. -" 'macrostep-expand)

(defvar ctl-dot-i-prefix-map)
(put 'ctl-dot-i-prefix-map 'variable-documentation "Input method map.")
(define-prefix-command 'ctl-dot-i-prefix-map)
(bind-key "C-. i" 'ctl-dot-i-prefix-map)
(bind-key "C-. i m" 'set-input-method)
(bind-key "C-. i e" 'toggle-input-method)
(bind-key "<XF86HomePage>" 'toggle-input-method)
(bind-key "C-. i s" (lambda () "Toggle on slovak-prog-2 input method." (interactive) (set-input-method "slovak-prog-2")))
(bind-key "C-. i c" (lambda () "Toggle on czech input method." (interactive) (set-input-method "czech")))
(bind-key "C-. i r" (lambda () "Toggle on russian-computer input method." (interactive) (set-input-method "russian-computer")))
(bind-key "C-. i q" (lambda () "Toggle on cyrillic-translit input method." (interactive) (set-input-method "cyrillic-translit")))
(bind-key "C-. i i" (lambda () "Toggle on italian-keyboard input method." (interactive) (set-input-method "italian-keyboard")))
(bind-key "C-. i g" (lambda () "Toggle on german input method." (interactive) (set-input-method "german")))
(bind-key "C-. i t" (lambda () "Toggle on TeX input method." (interactive) (set-input-method "TeX")))
(bind-key "C-. i l" (lambda () "Toggle on latin-macrons input method." (interactive) (set-input-method "latin-macrons")))

(bind-key "H-u" 'universal-argument)
(bind-key "H-u" 'universal-argument-more universal-argument-map)
(bind-key "H-0" 'digit-argument)
(bind-key "H-1" 'digit-argument)
(bind-key "H-2" 'digit-argument)
(bind-key "H-3" 'digit-argument)
(bind-key "H-4" 'digit-argument)
(bind-key "H-5" 'digit-argument)
(bind-key "H-6" 'digit-argument)
(bind-key "H-7" 'digit-argument)
(bind-key "H-8" 'digit-argument)
(bind-key "H-9" 'digit-argument)
(bind-key "H--" 'negative-argument)

(defvar ctl-c-s-map)
(define-prefix-command 'ctl-c-s-map)
(bind-key "C-c s" 'ctl-c-s-map)

(defvar ctl-c-m-map)
(define-prefix-command 'ctl-c-m-map)
(bind-key "C-c m" 'ctl-c-m-map)
(bind-key "C-c m b" 'magit-key-mode-popup-branching)
(bind-key "C-c m c" 'magit-key-mode-popup-committing)
(bind-key "C-c m d" 'magit-key-mode-popup-dispatch)
(bind-key "C-c m f" 'magit-key-mode-popup-fetching)
(bind-key "C-c m i" 'magit-key-mode-popup-diff-options)
(bind-key "C-c m l" 'magit-key-mode-popup-logging)
(bind-key "C-c m m" 'magit-key-mode-popup-merging)
(bind-key "C-c m p" 'magit-key-mode-popup-pushing)
(bind-key "C-c m v" 'magit-branch-manager)
(bind-key "C-c m s" 'magit-status)

;; zapping
(bind-key "M-z" 'zap-up-to-char)
(bind-key "M-Z" 'zap-to-char)

(defun my-quoted-insert-and-backward (arg)
  "Just like `quoted-insert' but moves the point before the
inserted character."
  (interactive "*p")
  (save-excursion
    (call-interactively 'quoted-insert)))

(bind-key "C-z" 'my-quoted-insert-and-backward)

;; M-s map
(bind-key "M-s RET" 'skeleton-easy-regexp-display-abbrev)
(bind-key "M-s c" 'calendar)

;; M-g map
(bind-key "M-g RET" 'skeleton-display-abbrev)


(bind-key "C-\\" 'toggle-input-method)
(bind-key "C-\\" 'my-cycle-language)
(defvar my-input-method :english)

(defun my-cycle-language ()
  (interactive)
  (case my-input-method
    (:slovak  (setq my-input-method :german)
              (my-toggle-input-method "german"))
    (:german  (setq my-input-method :english)
              (my-toggle-input-method nil))
    (:english (setq my-input-method :slovak)
              (my-toggle-input-method "slovak"))))

(defun my-toggle-input-method (new-input-method)
  (interactive)
  (if toggle-input-method-active
      (error "Recursive use of `toggle-input-method'"))
  (if (and current-input-method (not new-input-method))
      (inactivate-input-method)
    (let ((toggle-input-method-active t)
          (default (or (car input-method-history) default-input-method)))
      (if (and default (equal current-input-method default)
               (> (length input-method-history) 1))
          (setq default (nth 1 input-method-history)))
      (activate-input-method new-input-method))
    (unless default-input-method
      (prog1
          (setq default-input-method current-input-method)
        (when new-input-method
          (customize-mark-as-set 'default-input-method))))))

;; emms
;; (bind-key "<A-XF86AudioPlay>" 'emms-pause)
;; (require 'emms-setup)
;; (emms-standard)
;; (emms-default-players)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump to "logical" top/bottom of buffer in listing buffers

(defmacro my-special-buffer-back-to-top (mode &rest forms)
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-back-to-top")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-min))
           ,@forms
           (when (= p (point))
             (goto-char (point-min)))))
       (add-hook ',mode-hook (lambda () (define-key ,mode-map [remap beginning-of-buffer] ',fname))))))

(defmacro my-special-buffer-jump-to-bottom (mode &rest forms)
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-jump-to-bottom")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
           ,@forms
           (when (= p (point))
             (goto-char (point-max)))))
       (add-hook ',mode-hook (lambda () (define-key ,mode-map [remap end-of-buffer] ',fname))))))

(my-special-buffer-back-to-top dired
  (while (not (ignore-errors (dired-get-filename)))
    (dired-next-line 1)))
(my-special-buffer-jump-to-bottom dired
  (dired-previous-line 1))

(my-special-buffer-back-to-top occur
  (occur-next 1))
(my-special-buffer-jump-to-bottom occur
  (occur-prev 1))

(my-special-buffer-back-to-top ibuffer
  (ibuffer-forward-line 1))
(my-special-buffer-jump-to-bottom ibuffer
  (ibuffer-backward-line 1))

(my-special-buffer-back-to-top vc-dir
  (vc-dir-next-line 1))
(my-special-buffer-jump-to-bottom vc-dir
  (vc-dir-previous-line 1))

(my-special-buffer-back-to-top bs
  (bs-down 2))
(my-special-buffer-jump-to-bottom bs
  (bs-up 1)
  (bs-down 1))

(my-special-buffer-back-to-top recentf-dialog
  (my-recentf-next-file 3))
(my-special-buffer-jump-to-bottom recentf-dialog
  (my-recentf-previous-file 2))
