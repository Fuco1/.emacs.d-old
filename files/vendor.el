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
    (add-hook 'dired-mode-hook (lambda () (bind-key "M-o" 'elwm-activate-window dired-mode-map))))
  :config
  (progn
    (unbind-key "C-x o")))

(use-package expand-region
  :bind ("s-'" . er/expand-region))

(use-package google-maps
  :commands google-maps)

(use-package free-keys
  :commands free-keys)

(use-package jump-char
  :bind (("M-m" . jump-char-forward)))

(use-package keyadvice
  :commands keyadvice-mode)

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
