;;; Global key bindigns

;; setting the PC keyboard's various keys to
;; Super or Hyper, for emacs running on Windows.
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
(bind-key "M-<f2>" 'my-find-dired)
(bind-key "<f2>" 'occur)
(bind-key "n" 'occur-next occur-mode-map)
(bind-key "p" 'occur-prev occur-mode-map)

;; refresh-like
(bind-key "C-<f5>" 'revbufs)
(bind-key "<f5>" '(lambda () (interactive) (load-file (buffer-file-name))))

;; Indenting and alignment
(bind-key "<f8>" 'align-regexp)
(bind-key "C-<f8>" 'indent-buffer)
(bind-key "C-<tab>" 'indent-defun)

(bind-key "M-0" 'delete-window)
(bind-key "M-o" 'other-window)
(bind-key "M-O" 'rotate-windows)

(add-hook 'dired-mode-hook (lambda () (bind-key "M-o" 'other-window dired-mode-map)))
(add-hook 'ibuffer-mode-hook (lambda () (bind-key "M-o" 'other-window ibuffer-mode-map)))

(defvar f1-prefix-map)
(define-prefix-command 'f1-prefix-map)
(bind-key "<f1>" 'f1-prefix-map)
(bind-key "<f1> <f1>" 'ibuffer)
(bind-key "<f1> <f2>" (lambda () (interactive) (bookmark-jump "init.el")))
(bind-key "<f1> <f3>" (lambda () (interactive) (bookmark-jump "*Messages*")))
(bind-key "<f1> b" 'bs-show)

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
(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-K" (lambda () (interactive) (kill-buffer (window-buffer (next-window)))))

;; imenu
(bind-key "M-." 'ido-goto-symbol) ;; was Find tag
(bind-key "M-," 'find-function)

;; sexp settings
(bind-key "C-x e" 'eval-and-replace)
(bind-key "C-;" 'eval-expression)

;; minibuffer history
(bind-key "C-p" 'previous-history-element minibuffer-local-map)
(bind-key "C-n" 'next-history-element minibuffer-local-map)

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
;; binary search
(bind-key "H-b" 'chop-move-up)
(bind-key "H-f" 'chop-move-down)

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
(bind-key "C-c a" 'beginning-of-region)
(bind-key "C-c e" 'end-of-region)

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

;; go to char, also mapped to a keychord df
(bind-key "C-c f" 'iy-go-to-char)
(bind-key "C-c F" 'iy-go-to-char-backward)
(bind-key "C-c ;" 'iy-go-to-char-continue)
(bind-key "C-c ," 'iy-go-to-char-continue-backward)

;;; scrollers
(bind-key "C-c n" 'my-scroll-up)
(bind-key "C-c p" 'my-scroll-down)

;; deleting stuff
(bind-key "C-<i-key>" 'backward-kill-word)
(bind-key "C-<backspace>" 'my-kill-whitespace)

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

;; calc settings
(bind-key "<pause>" 'calc-dispatch)
(bind-key "<home>" 'calc-same-interface)

;; customize
(defvar customize-map)
(define-prefix-command 'customize-map)
(bind-key "C-c c" 'customize-map)
(bind-key "C-c c v" 'customize-variable)
(bind-key "C-c c f" 'customize-face)
(bind-key "C-c c g" 'customize-group)

(bind-key "C-c c c" 'my-mini-calc)

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)
(bind-key "C-h e" 'lisp-find-map)
(bind-key "C-h e F" 'find-face-definition)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)
(bind-key "C-h e v" 'find-variable)
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

(defvar ctl-c-r-map)
(define-prefix-command 'ctl-c-r-map)
(bind-key "C-c r" 'ctl-c-r-map)
(bind-key "C-c r f" 'recentf-open-files)
(bind-key "C-c r c" 'recentf-cleanup)

(defvar ctl-c-s-map)
(define-prefix-command 'ctl-c-s-map)
(bind-key "C-c s" 'ctl-c-s-map)
(bind-key "C-c s c" 'sp-convolute-sexp)
(bind-key "C-c s a" 'sp-absorb-sexp)
(bind-key "C-c s e" 'sp-emit-sexp)

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

;; input methods
;;(bind-key "C-\\" 'toggle-input-method)
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
         (beginning-of-buffer)
         ,@forms)
       (add-hook ',mode-hook (lambda () (define-key ,mode-map [remap beginning-of-buffer] ',fname))))))

(defmacro my-special-buffer-jump-to-bottom (mode &rest forms)
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-jump-to-bottom")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (end-of-buffer)
         ,@forms)
       (add-hook ',mode-hook (lambda () (define-key ,mode-map [remap end-of-buffer] ',fname))))))

(my-special-buffer-back-to-top dired
  (dired-next-line (if dired-omit-mode 2 4)))
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
