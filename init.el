(server-start)
(defconst emacs-start-time (current-time))

;; Emacs gurus don't need no stinking scroll bars & widgets
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; maximize window at startup
(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))
(add-hook 'window-setup-hook 'maximize-frame t)

;; add load paths
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")

;; add repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(require 'autoinstall)
(require 'parenface)
(projectile-global-mode t)

(load "files/defuns") ;; for vendor
(vendor 'use-package)
(recentf-mode t)

(vendor 'keyadvice)

;; autoloads
(autoload 'calc-same-interface "calc" nil t)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; load config files
(load "files/org-mode")
(load "files/global")
(load "files/defuns-edit")
(load "files/defuns-buffer")
(load "files/defuns-macros")
(load "files/advices")
(load "files/macros")
(load "files/keys")
(load "files/isearch")
(load "files/tabs")
(load "files/ido")
(load "files/ispell")
(load "files/dired-setup")
(load "files/markdown")
(load "files/tramp")
(load "files/ibuffer")
(load "files/undo-tree")
(load "files/expand-region")
(load "files/emacs-lisp-mode")
(load "files/multi-web-mode")
(load "files/latex-mode")
(load "files/mode-line")
(load "files/smex")
(load "files/ack")
(load "files/allout")
(load "files/navigation")
(load "files/haskell")
(load "c:/Users/Matus/AppData/Roaming/.emacs.d/dev/shell-pop-el/shell-pop")

;; vendor
(vendor 'yasnippet)
(vendor 'typopunct 'typopunct-change-language 'typopunct-mode)
(vendor 'wc-mode)
(vendor 'revbufs 'revbufs)
(vendor 'golden-ratio)
;;(vendor 'iy-go-to-char 'iy-go-to-char 'iy-go-to-char-backward)
(vendor 'smartparens)
(vendor 'letcheck 'letcheck-mode)
(vendor 'chop 'chop-move-up 'chop-move-down)
(vendor 'clippy 'clippy-describe-function)
(vendor 'google-maps 'google-maps)
(vendor 'elwm)
(vendor 'skeleton-complete)
(skeleton-complete-global-mode t)
(define-key skeleton-complete-mode-map (kbd "M-g RET") 'skeleton--expand-symbols)
(define-key skeleton-complete-mode-map (kbd "M-s RET") 'skeleton--expand-partial-lines)
(vendor 'flex-isearch)
(global-flex-isearch-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(use-package "world-time-mode"
  :load-path "vendor/world-time-mode/"
  :bind ("C-. t" . world-time-list))

;; Diminish modeline clutter
(load "files/diminish")

;; Customize
(setq custom-file "~/.emacs.d/files/emacs-custom.el")
(load custom-file)

;; Reload theme -- hackish
(load "~/.emacs.d/themes/my-tango-dark-theme")

;; autoopen files
(find-file "~/.emacs.d/init.el")

;;; post init.
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;; removed packages:
;; sunrise-commander
