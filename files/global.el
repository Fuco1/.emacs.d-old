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

;; Gotta see matching parens
(show-paren-mode t)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; Trash can support
(setq delete-by-moving-to-trash t)

;; zap-up-to-char, forward-to-word, backward-to-word, etc
(require 'misc)

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

;; pending deletion. Replace active region with input
(pending-delete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some text-mode settings

;; turn on visual-line-mode for text-mode major mode
;; (add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(setq-default cursor-type 'box)
;; variable width font in text buffers ...
(dolist (hook '(text-mode-hook
                LaTeX-mode-hook
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
  (progn (my-buffer-face-mode-variable)
         (setq cursor-type 'bar)
         (setq indent-tabs-mode t)
         (turn-on-visual-line-mode)
         (require 'typopunct)
         (typopunct-change-language 'english)
         (typopunct-mode 1)))

;; ... but not in Org tables
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)

;; (custom-set-faces
;;  '(variable-pitch ((t (:family "Bitstream Vera Serif")))))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("emacs-config"
                (or (filename . ".emacs.d")
                    (filename . ".emacs")
                    (filename . "emacs-config")))
               ("TeX"
                (or (mode . tex-mode)
                    (mode . TeX-mode)
                    (mode . latex-mode)
                    (mode . LaTeX-mode)
                    (filename . ".tex")))
               ("Web"
                (or (mode . html-mode)
                    (mode . css-mode)
                    (mode . PHP-mode)))
               ("Sunrise"
                (or (mode . sr-mode)))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . c-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . cc-mode)
                 (mode . js-mode)
                 ;; etc
                 ))))))

(defun customize-ibuffer-mode ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (setq ibuffer-hidden-filter-groups
        (push "Sunrise" ibuffer-hidden-filter-groups)))
(add-hook 'ibuffer-mode-hook 'customize-ibuffer-mode)
