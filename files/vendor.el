(require 'bookmark+-autoloads)

(use-package ace-jump-mode
  :bind (("C-\\" . ace-jump-mode))
  :config
  (progn
    (setq ace-jump-mode-scope 'window)))

;; (use-package achievements-mode
;;   :commands achievements-mode)

(use-package clippy
  :commands clippy-describe-function)

(use-package elwm
  :bind (("M-o" . elwm-activate-window)
         ("M-O" . elwm-transpose-window)
         ("C-M-o" . elwm-rotate-window)
         ("C-x C-2" . elwm-split-window))
  :init
  (progn
    (add-hook 'dired-mode-hook (lambda () (bind-key "M-o" 'elwm-activate-window dired-mode-map)))))

(use-package ediff
  :pre-init
  (progn
    (defvar ctl-dot-equals-prefix-map)
    (define-prefix-command 'ctl-dot-equals-prefix-map)
    (bind-key "C-. =" 'ctl-dot-equals-prefix-map))
  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))
  :config
  (progn
    (defvar my-ediff-before-config nil "Window configuration before ediff.")
    (defvar my-ediff-after-config nil "Window configuration after ediff.")

    (defun my-ediff-before-setup ()
      "Function to be called before any buffers or window setup for
    ediff."
      (setq my-ediff-before-config (current-window-configuration))
      (set-register ?b (list my-ediff-before-config (point-marker))))

    (defun my-ediff-after-setup ()
      "Function to be called after buffers and window setup for ediff."
      (setq my-ediff-after-config (current-window-configuration))
      (set-register ?e (list my-ediff-after-config (point-marker))))

    (defun my-ediff-quit ()
      "Function to be called when ediff quits."
      (when my-ediff-before-config
        (set-window-configuration my-ediff-before-config))
      ;; clean up ediff bullshit
      (->> (buffer-list)
        (-map 'buffer-name)
        (--select (string-match-p "\\*[Ee]diff" it))
        (-map 'kill-buffer)))

    (add-hook 'ediff-before-setup-hook 'my-ediff-before-setup)
    (add-hook 'ediff-after-setup-windows-hook 'my-ediff-after-setup 'append)
    (add-hook 'ediff-quit-hook 'my-ediff-quit)))

(use-package expand-region
  :bind ("s-'" . er/expand-region))

(use-package golden-ratio
  :config
  (progn
    (defun my-golden-ratio-inhibit ()
      (or (--any? (string-match-p "\\*Ediff Control Panel" it)
                  (mapcar 'buffer-name (mapcar 'window-buffer (window-list))))
          ))))

(use-package google-maps
  :commands google-maps)

(use-package guide-key
  :commands guide-key-mode
  :config
  (progn
    ;; (defadvice guide-key/popup-guide-buffer (around fix-width activate)
    ;;   (let ((window (car (get-buffer-window-list " *guide-key*")))
    ;;         (config popwin:popup-last-config))
    ;;     ad-do-it
    ;;     (save-window-excursion
    ;;       (select-window window)
    ;;       (goto-char (point-max))
    ;;       (let ((cl (line-number-at-pos))
    ;;             (wh (window-height window)))
    ;;         (message "%d %d" cl wh)
    ;;         (when (< cl wh)
    ;;           (window-resize window (- (- wh cl))))))
    ;;     (setq popwin:popup-last-config config)))
    ))

(use-package free-keys
  :commands free-keys)

(use-package jump-char
  :bind (("M-m" . jump-char-forward)))

(use-package keyadvice
  :defer t
  :init (progn (load "vendor/keyadvice.el/autoloads.el")))

(use-package keyfreq
  :bind ("C-. C-k" . keyfreq-show)
  :config
  (progn
    ;; hack to make nicer format in keyfreq-show
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

(use-package letcheck
  :commands letcheck-mode)

(use-package magit
  :defer t
  :config
  (progn
    (require 'flyspell)))

(use-package popwin
  :commands popwin-mode
  :config
  (progn
    (push '("*Pp Eval Output*" :height 15) popwin:special-display-config)))

(use-package revbufs
  :bind ("C-<f5>" . revbufs))

(use-package smartparens
  :defer t
  :init
  (progn
    (load "~/.emacs.d/files/smartparens")))

(use-package smex
  :defer t
  :init
  (progn
    (bind-key "M-x" 'beautify-smex)
    (bind-key "M-X" 'smex-major-mode-commands)
    ;; This is your old M-x.
    (bind-key "C-c C-c M-x" 'execute-extended-command)

    (defun beautify-smex ()
      (interactive)
      (unwind-protect
          (progn
            (setq ido-decorations
                  '("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
            (smex))
        (setq ido-decorations
              '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))))
  :config
  (progn
    (defun smex-update-after-load (unused)
      (when (boundp 'smex-cache)
        (smex-update)))
    (add-hook 'after-load-functions 'smex-update-after-load)

    (defun smex-prepare-ido-bindings ()
      (define-key ido-completion-map (kbd "C-h f") 'smex-describe-function)
      (define-key ido-completion-map (kbd "C-h w") 'smex-where-is)
      (define-key ido-completion-map (kbd "M-.") 'smex-find-function)
      (define-key ido-completion-map (kbd "C-a") 'move-beginning-of-line)
      (define-key ido-completion-map (kbd "=") "-"))))

(use-package wc-mode
  :commands wc-mode)

(use-package wiktionary-translate
  :bind ("<insert>" . wd-show-translation))

(use-package "world-time-mode"
  :bind ("C-. t" . world-time-list))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode
             yas-expand)
  :init
  (progn
    (autoload 'yas/hippie-try-expand "yasnippet"))
  :config
  (progn
    (require 'dropdown-list)

    (yas-global-mode 1)

    (setq yas-snippet-dirs '("~/.emacs.d/vendor/yasnippet/snippets"))

    (setq yas-prompt-functions '(yas-ido-prompt))

    (defun my-yas-startup ()
      ;; stupid yasnippet :/ we define <tab> behaviour elsewhere
      (define-key yas-minor-mode-map [(tab)] nil)
      (define-key yas-minor-mode-map (kbd "TAB") nil))

    ;; Replace yasnippets's TAB, was yas/expand
    (add-hook 'yas-minor-mode-hook 'my-yas-startup)))
