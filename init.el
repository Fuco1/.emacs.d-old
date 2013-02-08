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
(vendor 'use-package)

;; autoloads
(autoload 'calc-same-interface "calc" nil t)
(autoload 'dired-w32-browser "w32-browser" nil t)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; load config files
(load "files/org-mode")
(load "files/global")
(load "files/defuns-edit")
(load "files/defuns-buffer")
(load "files/defuns")
(load "files/advices")
(load "files/macros")
(load "files/keys")
(load "files/isearch")
(load "files/tabs")
(load "files/ido")
(load "files/ispell")
(load "files/dired")
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

;; vendor
(vendor 'yasnippet)
(vendor 'typopunct 'typopunct-change-language 'typopunct-mode)
(vendor 'wc-mode)
(vendor 'revbufs 'revbufs)
(vendor 'shell-pop 'shell-pop)
(vendor 'golden-ratio)
(vendor 'iy-go-to-char 'iy-go-to-char 'iy-go-to-char-backward)
(vendor 'sunrise-commander)
(vendor 'smartparens)
(vendor 'letcheck 'letcheck-mode)

;; Diminish modeline clutter
(load "files/diminish")

;restore the s-c tabs
(load "files/desktop")

;; TEX
(require 'tildify)

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
