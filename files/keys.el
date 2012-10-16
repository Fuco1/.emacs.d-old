
;;; Global key bindigns
;; How to Define Keyboard Shortcuts in Emacs
;; http://xahlee.org/emacs/keyboard_shortcuts.html

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

;; Keyboard macros
(global-set-key [(shift f4)] 'kmacro-start-macro-or-insert-counter)
;; (global-set-key [(f4)]    'kmacro-end-or-call-macro)  ;; already defined

;; Refresh-like
;(global-set-key [(f5)]         'revert-buffer)
;(global-set-key [(control f5)] 'revbufs)
(global-set-key [(f5)] '(lambda () (interactive) (load-file (buffer-file-name))))

;; Indenting and alignment
(global-set-key [(f8)]         'indent-region)
(global-set-key [(control f8)] 'align)
(global-set-key [(shift f8)]   'align-current)
(global-set-key [(meta f8)]    'align-regexp)

;; regex search & replace
(global-set-key [(f6)]         'replace-regexp)
(global-set-key [(f7)]         'isearch-forward-regexp)

;; map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap

(global-set-key (kbd "M-O") 'rotate-windows)

;; Replace dired's M-o
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "M-o") 'other-window))) ; was dired-omit-mode
;; Replace ibuffer's M-o
(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map (kbd "M-o") 'other-window))) ; was ibuffer-visit-buffer-1-window
;; To help Unlearn C-x 0, 1, 2, o
(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "C-x 0")) ; was delete-window
(global-unset-key (kbd "C-x o")) ; was other-window

;; ibuffer > list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

; buffer switching
(global-set-key (kbd "M-]") 'previous-buffer)
(global-set-key (kbd "M-\\") 'next-buffer)

;; Window navigation
(windmove-default-keybindings 'meta)

;; Mac OS X conventions
(global-set-key (kbd "M-a") 'mark-whole-buffer) ; was backward-sentence.
(add-hook 'prolog-mode-hook (lambda () (define-key prolog-mode-map (kbd "M-a") 'mark-whole-buffer)))

;; Find matching parens
;; (global-set-key (kbd "C-'") 'match-paren)

;; Easier buffer killing
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'kill-this-buffer)

;; imenu
(global-unset-key (kbd "M-.")) ;; was Find tag
;;(global-set-key (kbd "M-.") 'imenu)
(global-set-key (kbd "M-.") 'ido-goto-symbol)

;; jump to beginning/end of the file
(global-unset-key [(home)]) ;; was C-a
(global-unset-key [(end)]) ;; was C-e
(global-set-key [(home)] 'beginning-of-buffer)
(global-set-key [(end)] 'end-of-buffer)

(defun select-to-the-beginning-of-line ()
  (interactive)
  (set-mark (point))
  (beginning-of-line))

(global-set-key (kbd "C-x a") 'select-to-the-beginning-of-line)

(defun copy-line ()
  (interactive)
  (let ((x (point)))
    (kill-line)
    (yank)
    (goto-char x)))

(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v") 'copy-line)

(global-set-key (kbd "C-x C-g") 'goto-line)
