;;; Global settings
;;; Generic emacs settings I cannot live without

;; Winner mode
;; C-c left C-c right switch between window configurations, M-arrows to jump between windows
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;; Modeline info
(display-time-mode 1)
;; (display-battery-mode 1)

;; Small fringes
(set-fringe-mode '(1 . 1))

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)

;; Prevent the annoying beep on errors
(setq visible-bell t)

;; Make sure all backup files only live in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook 'cleanup-buffer-safe)
(add-hook 'before-save-hook 'my-create-directory-on-save)

;; Trash can support
(setq delete-by-moving-to-trash t)

;; Syntax hilight
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; UTF
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq calendar-week-start-day 1)

(setq message-log-max 10000)

;; cua-selection-mode for all the goodies ;)
(cua-selection-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some text-mode settings

;; turn on visual-line-mode for text-mode major mode
;; (add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(setq-default cursor-type 'box)
;; variable width font in text buffers ...
(dolist (hook '(LaTeX-mode-hook
                org-mode-hook
                markdown-mode-hook
                gnus-article-mode-hook))
  (add-hook hook 'init-text-based-modes))

;; Use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Verdana" :height 120))
  (buffer-face-mode))
;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Consolas" :height 100))
  (buffer-face-mode))

(defun init-text-based-modes ()
  (my-buffer-face-mode-variable)
  (setq cursor-type 'bar)
  (setq indent-tabs-mode t)
  (turn-on-visual-line-mode))

;; move this somewhere appropriate
(eval-after-load "bs"
  '(progn
     (defvar bs-mode-font-lock-keywords
       (list ;; header in font-lock-type-face
        (list (bs--make-header-match-string)
              '(1 font-lock-type-face append) '(1 'bold append))
        ;; Buffername embedded by *
        (list "^.*\\(\\*.*\\*\\).*$" 1 'font-lock-comment-face)
        ;; Dired-Buffers
        '("^....\\(.*\\) [0-9].*Dired[ /].*$" 1 font-lock-function-name-face)
        ;; the star for modified buffers
        '("^.\\(\\*\\) +[^\\*]"     1 font-lock-warning-face))
       "Default font lock expressions for Buffer Selection Menu.")))
