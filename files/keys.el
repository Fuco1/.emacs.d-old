;;; Global key bindigns
;; How to Define Keyboard Shortcuts in Emacs
;; http://xahlee.org/emacs/keyboard_shortcuts.html

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
(global-set-key (kbd "C-c C-w") 'browse-url-at-point)
(global-set-key (kbd "C-c w") 'browse-url)

;; Window manipulation
(global-set-key [(control kp-6)] 'enlarge-window-horizontally)
(global-set-key [(control kp-4)] 'shrink-window-horizontally)
(global-set-key [(control kp-8)] 'enlarge-window)
(global-set-key [(control kp-2)] 'shrink-window)

;; Find stuff
(global-set-key [(f2)]              'ack-default-directory)
(global-set-key [(control f2)]      'ack-same)
(global-set-key [(control meta f2)] 'ack)
(global-set-key [(meta f2)]         'find-name-dired)
(global-set-key [(shift f2)]        'occur)

;; refresh-like
(global-set-key [(control f5)] 'revbufs)
(global-set-key [(f5)] '(lambda () (interactive) (load-file (buffer-file-name))))

;; Indenting and alignment
(global-set-key [(f8)]         'indent-region)
(global-set-key [(control f8)] 'indent-buffer)
(global-set-key [(shift f8)]   'align-current)
(global-set-key [(meta f8)]    'align-regexp)

;; shell pop
(global-set-key [(f9)] 'shell-pop)

;; map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically)   ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows)      ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window)             ; was digit-argument
(global-set-key (kbd "M-o") 'other-window)              ; was facemenu-keymap

(global-set-key (kbd "M-O") 'rotate-windows)

;; Move windows, even in org-mode (doesn't work on windows :( )
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Replace dired's M-o
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "M-o") 'other-window))) ; was dired-omit-mode
;; Replace ibuffer's M-o
(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map (kbd "M-o") 'other-window))) ; was ibuffer-visit-buffer-1-window

;; ibuffer > list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; buffer cleanup
(global-set-key (kbd "C-c u") 'cleanup-buffer)

;; Window navigation
(windmove-default-keybindings 'meta)

;; Easier buffer killing
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-K") (lambda () (interactive) (kill-buffer (window-buffer (next-window)))))

;; imenu
(global-unset-key (kbd "M-.")) ;; was Find tag
(global-set-key (kbd "M-.") 'ido-goto-symbol)
(global-set-key (kbd "M-,") 'find-function)

;; sexp settings
(global-set-key (kbd "C-x e") 'eval-and-replace)
(global-set-key (kbd "C-;") 'eval-expression)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation

;; remove arrows, evil!
(mapc 'global-unset-key [[up] [down] [left] [right]])

;; # move by (forw/back)
;; character
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-b") 'backward-char)
;; word
(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)
;; line
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-p") 'previous-line)
;; sentence
(global-set-key (kbd "M-e") 'forward-sentence)
(global-set-key (kbd "M-a") 'backward-sentence)
;; paragraph
(global-unset-key (kbd "M-{")) ;; was backward-paragraph
(global-unset-key (kbd "M-}")) ;; was forward-paragraph
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-{") 'backward-paragraph-select)
(global-set-key (kbd "M-}") 'forward-paragraph-select)
;; screen
(global-set-key (kbd "C-v") 'scroll-up-command)
(global-set-key (kbd "M-v") 'scroll-down-command)
;; sexp
(global-set-key (kbd "C-M-f") 'forward-sexp)
(global-set-key (kbd "C-M-b") 'backward-sexp)
;; list
(global-set-key (kbd "C-M-n") 'forward-list)
(global-set-key (kbd "C-M-p") 'backward-list)
;; semantic unit
(global-set-key (kbd "M-\\") 'smart-forward)
(global-set-key (kbd "M-'") 'smart-backward)

;; # move to (beg/end)
;; line
;; swap C-a and M-m, back-to-indentation is much more common
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "C-a"))
(global-set-key (kbd "M-m") 'move-beginning-of-line)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)
(global-set-key (kbd "C-e") 'end-of-code-or-line)
;; buffer
(global-unset-key [(home)]) ;; was C-a
(global-unset-key [(end)]) ;; was C-e
(global-set-key (kbd "M-<") 'beginning-of-buffer)
(global-set-key (kbd "C-M-,") 'beginning-of-buffer)
(global-set-key (kbd "M->") 'end-of-buffer)
(global-set-key (kbd "C-M-.") 'end-of-buffer)

;; defun
(global-set-key (kbd "M-p") 'beginning-of-defun)
(global-set-key (kbd "M-n") 'end-of-defun)
;; active region
(global-set-key (kbd "C-c a") 'beginning-of-region)
(global-set-key (kbd "C-c e") 'end-of-region)

;; # move into
(global-set-key (kbd "C-M-d") 'down-list)
(global-set-key (kbd "C-M-e") 'up-list+)
(global-set-key (kbd "C-M-u") 'backward-up-list+)

;; line selection
(global-set-key (kbd "H-a") 'beginning-of-line-select)
(global-set-key (kbd "H-e") 'end-of-line-select)
(global-set-key (kbd "H-s") 'line-select)

(global-set-key (kbd "M-i") 'change-inner)

(global-set-key (kbd "H-w") 'copy-line)
(global-set-key (kbd "C-c <up>") 'copy-previous-line)
(global-set-key (kbd "C-c <down>") 'copy-next-line)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; opening new lines. C-o can be called from any point on the line
;; ret from the end only
(global-set-key (kbd "RET") 'open-next-line)
(global-set-key (kbd "C-o") 'vi-open-next-line)
(global-set-key (kbd "C-S-o") 'forward-line-and-indent)

;; go to char, also mapped to a keychord df
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward)

;;; scrollers
(global-set-key (kbd "C-c n") 'my-scroll-up)
(global-set-key (kbd "C-c p") 'my-scroll-down)

;; deleting stuff
(global-set-key (kbd "C-<i-key>") 'backward-kill-word)

;; keys for specific modes
(defun add-html-binding ()
  (define-key (current-local-map) (kbd "C-c <deletechar>") 'sgml-delete-tag))
(dolist (hook '(sgml-mode-hook
                html-mode-hook))
  (add-hook hook 'add-html-binding))

;; sunrise
(global-set-key [(f1)] 'sunrise)
(global-set-key [(meta f1)] 'sunrise-cd)

;; mark commands
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; calc settings
(global-set-key (kbd "<pause>") 'calc-dispatch)
(global-set-key (kbd "<home>") 'calc-same-interface)
(global-set-key (kbd "C-c c") 'my-mini-calc)

;; you can select the key you prefer to (hyperfind!)
(define-key global-map (kbd "H-f") 'ace-jump-mode)

;; zapping
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; input methods
;;(global-set-key (kbd "C-\\") 'toggle-input-method)
(global-set-key (kbd "C-\\") 'my-cycle-language)
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
          (customize-mark-as-set 'default-input-method)))))))
