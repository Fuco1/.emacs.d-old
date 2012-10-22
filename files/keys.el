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

;; Keyboard macros
(global-set-key [(shift f4)] 'kmacro-start-macro-or-insert-counter)

;; Refresh-like
(global-set-key [(control f5)] 'revbufs)
(global-set-key [(f5)] '(lambda () (interactive) (load-file (buffer-file-name))))

;; Indenting and alignment
(global-set-key [(f8)]         'indent-region)
(global-set-key [(control f8)] 'align)
(global-set-key [(shift f8)]   'align-current)
(global-set-key [(meta f8)]    'align-regexp)

;; regex search & replace
(global-set-key [(f6)] 'replace-regexp)
(global-set-key [(f7)] 'isearch-forward-regexp)

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
;; To help Unlearn C-x 0, 1, 2, o
(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "C-x 0")) ; was delete-window

;; ibuffer > list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

; buffer switching
(global-set-key (kbd "M-]") 'previous-buffer)
(global-set-key (kbd "M-\\") 'next-buffer)

;; Window navigation
(windmove-default-keybindings 'meta)

;; Easier buffer killing
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-K") (lambda () (interactive) (kill-buffer (window-buffer (next-window)))))

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
;; swap C-a and M-m
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "C-a"))
(global-set-key (kbd "M-m") 'beginning-of-line)
(global-set-key (kbd "C-a") 'back-to-indentation)

(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (if (looking-at "^")
      (open-line arg)
    (end-of-line)
    (open-line arg)
    (next-line 1)
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'vi-open-next-line)

(defun copy-line ()
  (interactive)
  (let ((x (point)))
    (kill-line)
    (yank)
    (goto-char x)))

(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v") 'copy-line)

(global-set-key (kbd "C-x C-g") 'goto-line)

(global-set-key (kbd "C-M-a") 'backward-sexp)
(global-set-key (kbd "C-M-e") 'forward-sexp)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(defun open-next-line ()
  (interactive)
  (newline)
  (indent-according-to-mode))

(global-set-key (kbd "RET") 'open-next-line)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward)


;; 0 = previous, 2 = next
;; but we map it to +/-1 to be more intuitive
;; +1 -> 2
;; -1 -> 0
(defun copy-line-with-offset (offset)
  (kill-ring-save (line-beginning-position (+ offset 1))
                  (line-end-position (+ offset 1)))
  (let ((pos (point))
        (line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (beginning-of-line)
    (when (or (and (string-match "[:space:]" line)
                    (> offset 0))
              (< offset 0))
      (newline)
      (forward-line -1))
    (beginning-of-line)
    (insert (car kill-ring))
    (goto-char pos)))

(defun copy-previous-line ()
  (interactive)
  (copy-line-with-offset -1))

(defun copy-next-line ()
  (interactive)
  (copy-line-with-offset 1))

(global-set-key (kbd "C-c <up>") 'copy-previous-line)
(global-set-key (kbd "C-c <down>") 'copy-next-line)
