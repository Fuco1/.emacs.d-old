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
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(mapc (apply-partially 'add-to-list 'load-path) (f-directories "~/.emacs.d/vendor"))

;; add repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(require 'autoinstall)
(package-initialize)

(require 'use-package)
(require 'parenface)
(require 'uniquify)
(require 'dash)
(require 'f)
(require 's)

;; autoloads
(autoload 'calc-same-interface "calc" nil t)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; load site lisp
(mapc 'load (f-files "~/.emacs.d/site-lisp/"))

;; load settings
(load "files/global")
(load "files/layouts")
(load "files/mode-line")
(load "files/tabs")
(load "files/windows")

;; load config files
(load "files/ack")
(load "files/allout")
(load "files/auto-complete")
(load "files/dired-setup")
(load "files/eshell-mode")
(load "files/haskell")
(load "files/ibuffer-config")
(load "files/ido")
(load "files/isearch")
(load "files/ispell")
(load "files/latex-mode-config")
(load "files/markdown")
(load "files/multi-web-mode-config")
(load "files/org-mode")
(load "files/recentf-config")
(load "files/tramp")
(load "files/vendor")
(load "c:/Users/Matus/AppData/Roaming/.emacs.d/dev/shell-pop-el/shell-pop")

;; diminish useless modeline clutter
(load "files/diminish")

;; load keys
(load "files/keys")

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
