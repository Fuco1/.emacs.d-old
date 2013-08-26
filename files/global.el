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

(setq edebug-inhibit-emacs-lisp-mode-bindings t)

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
     (setq bs-mode-font-lock-keywords
           (list ;; header in font-lock-type-face
            (list (bs--make-header-match-string)
                  '(1 font-lock-type-face append) '(1 'bold append))
            ;; Buffername embedded by *
            (list "^.*\\(\\*.*\\*\\).*$" 1 'font-lock-comment-face)
            ;; Dired-Buffers
            '("^....\\(.*\\) [0-9].*Dired[ /].*$" 1 font-lock-function-name-face)
            ;; the star for modified buffers
            '("^.\\(\\*\\) +[^\\*]"     1 font-lock-warning-face)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC

(setq global-mode-string '("" erc-modified-channels-object))

;; special colors for some people
(setq erc-nick-color-alist '(;; ##latin
                             ("theseb" . "dodger blue")
                             ("godmy" . "indian red")
                             ("fuco" . "SkyBlue1") ;;#8cc4ff
                             ("rhemaxx0s" . "medium sea green")
                             ("tonitrus" . "forest green")
                             ;; #emasc
                             ("wgreenhouse" . "green")
                             ("nicferrier" . "indian red")
                             ("ijp" . "cornflower blue")
                             ("forcer" . "forest green")
                             ("fsbot" . "gray35")
                             ("fsbot`" . "gray35")
                             ("jlf" . "NavajoWhite4")
                             ("taylanub" . "DeepSkyBlue1")
                             ("jordigh" . "orchid1")
                             ))

(defun erc-get-color-for-nick (nick)
  "Gets a color for NICK. If NICK is in erc-nick-color-alist, use
that color, else hash the nick and use a random color from the
pool"
  (or (cdr (assoc (downcase nick) erc-nick-color-alist))
      "#edd400"))

(defun erc-put-color-on-nick ()
  "Modifies the color of nicks according to erc-get-color-for-nick"
  (let (word bounds)
    (save-excursion
      (let ((inhibit-read-only t)) (put-text-property (point-min) (point-max) 'read-only nil))
      (goto-char (point-min))
      (while (or (forward-word 1) (not (eobp)))
        (setq bounds (bounds-of-thing-at-point 'word))
        (when bounds
          (setq word (buffer-substring-no-properties
                      (car bounds) (cdr bounds)))
          (when (or (and (erc-server-buffer-p) (erc-get-server-user word))
                    (and erc-channel-users (erc-get-channel-user word)))
            (put-text-property (car bounds) (cdr bounds)
                               'face (cons 'foreground-color
                                           (erc-get-color-for-nick word)))
            ))))))

(add-hook 'erc-insert-post-hook 'erc-put-color-on-nick)

(defun my-erc-init ()
  (modify-syntax-entry ?\_ "w" nil)
  (modify-syntax-entry ?\- "w" nil)
  (modify-syntax-entry ?` "w" nil))
(add-hook 'erc-mode-hook 'my-erc-init)

(defun my-erc-join-channel ()
  (smartparens-mode -1)
  (show-smartparens-mode -1)
  (visual-line-mode 1)
  (erc-fill-mode -1))
(add-hook 'erc-join-hook 'my-erc-join-channel)

(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-full-name
                      channels))))

;; hack to make nicer format in keyfreq-show
(eval-after-load 'keyfreq
  '(progn
     (defun keyfreq-format-list (list &optional func)
       "Returns formatted string with command usage statistics.

The LIST is the `keyfreq-table' converted to a list using the `keyfreq-list'.

If FUNC is nil each line contains number of times command was
called and the command; if it is t percentage usage is added in
the middle; if it is 'raw each line will contain number an
command separated by single line (with no formatting) otherwise
FUNC must be a function returning a string which will be called
for each entry with three arguments: number of times command was
called, percentage usage and the command."
       (let* ((sum (car list)))
         (mapconcat
          (cond
           ((not func) (lambda (e) (format "%7d  %s\n" (cdr e) (car e))))
           ((equal func t)
            (lambda (e) (format "%7d  %6.2f%% %10s %s\n"
                                (cdr e) (/ (* 1e2 (cdr e)) sum) (key-description (where-is-internal (car e) nil t)) (car e))))
           ((equal func 'raw) (lambda (e) (format "%d %s\n" (cdr e) (car e))))
           (t (lambda (e) (funcall func (cdr e) (/ (* 1e2 (cdr e)) sum) (car e)))))
          (cdr list) "")))))
