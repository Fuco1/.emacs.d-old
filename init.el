(server-start)

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

;; load config files
(load "files/org-mode")
(load "files/global")
(load "files/defuns-edit")
(load "files/defuns-buffer")
(load "files/defuns")
(load "files/advices")
(load "files/macros")
(load "files/keys")
(load "files/tabs")
(load "files/ido")
(load "files/ispell")
(load "files/dired")

(load "files/undo-tree")
(load "files/expand-region")

;; vendor
(vendor 'smex)
(vendor 'yasnippet)
(vendor 'haskell-mode)
(vendor 'typopunct)
(vendor 'wc-mode)
(vendor 'ace-jump-mode)
(vendor 'revbufs)
(vendor 'shell-pop)
(vendor 'golden-ratio)
(vendor 'multiple-cursors)
(vendor 'iy-go-to-char)
(vendor 'sunrise-commander)
(vendor 'change-inner)
(vendor 'smart-forward)
(vendor 'smartparens)

(require 'parenface)

;restore the s-c tabs
(load "files/desktop")

;; TEX
(require 'tildify)

;; autoopen files
(find-file "~/.emacs.d/init.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ange-ftp-ftp-program-name "d:\\progs\\ftp.exe")
 '(background-color "#002b36")
 '(background-mode dark)
 '(blink-cursor-mode nil)
 '(calc-settings-file "~\\.emacs.d\\my-calc.el")
 '(column-number-mode t)
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(desktop-buffer-filter (quote my-desktop-filter))
 '(desktop-save t)
 '(display-time-mode t)
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-nick "Fuco")
 '(erc-prompt ">")
 '(erc-timestamp-format "[%H:%M:%S] ")
 '(erc-timestamp-format-left "[%H:%M:%S] ")
 '(erc-timestamp-format-right " [%H:%M:%S]")
 '(foreground-color "#839496")
 '(global-undo-tree-mode t)
 '(ido-ubiquitous-function-exceptions (quote (grep-read-files\,beautify-smex\,smex-major-mode-commands\,ido-goto-symbol)))
 '(ido-ubiquitous-mode t)
 '(imenu-auto-rescan t)
 '(ls-lisp-dirs-first t)
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(sr-attributes-display-mask (quote (nil nil nil nil t t t)))
 '(sr-listing-switches "-alh")
 '(sr-popviewer-enabled nil)
 '(sr-show-file-attributes nil)
 '(sr-tabs-max-tabsize 18)
 '(sr-traditional-other-window nil)
 '(sr-windows-locked nil)
 '(text-mode-hook (quote (init-text-based-modes text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 98 :width normal))))
 '(comint-highlight-input ((t (:foreground "cyan"))))
 '(comint-highlight-prompt ((t (:foreground "cyan"))))
 '(diff-added ((t (:foreground "lime green"))))
 '(diff-changed ((t (:foreground "orange"))))
 '(diff-file-header ((((class color) (background light)) (:background "lightblue" :bold t))))
 '(diff-header ((nil (:foreground "skyblue"))))
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "navyblue"))))
 '(diff-removed ((t (:foreground "orangered"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "lime green"))))
 '(dropdown-list-face ((t (:inherit default :background "lightyellow" :foreground "black" :family "Consolas"))))
 '(erc-action-face ((t (:foreground "green"))))
 '(erc-current-nick-face ((t (:foreground "red" :weight bold))))
 '(erc-input-face ((t (:foreground "cyan"))))
 '(erc-my-nick-face ((t (:foreground "cyan"))))
 '(erc-nick-default-face ((t (:foreground "yellow" :weight bold))))
 '(erc-prompt-face ((t (:background "Black" :foreground "Grey" :weight bold))))
 '(erc-timestamp-face ((t nil)))
 '(eshell-prompt ((t (:foreground "SlateBlue2" :weight bold))))
 '(fixed-pitch ((t (:height 100 :family "Consolas"))))
 '(fringe ((((class color) (background light)) (:background "grey95" :foreground "black"))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2" :foreground "black"))))
 '(hl-line ((t (:background "gray12"))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face :family "Consolas"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(org-table ((t (:inherit fixed-pitch :foreground "LightSkyBlue"))))
 '(paren-face ((t (:foreground "thistle4"))))
 '(region ((((class color) (min-colors 88) (background light)) (:background "color-56"))))
 '(sr-active-path-face ((t (:background "gray19" :foreground "deep sky blue"))))
 '(sr-editing-path-face ((t (:background "red" :foreground "yellow"))))
 '(sr-highlight-path-face ((t (:background "yellow" :foreground "#ace6ac"))))
 '(sr-mirror-path-face ((t (:background "blue" :foreground "yellow"))))
 '(sr-passive-path-face ((t (:background "gray19" :foreground "lightgray"))))
 '(sr-tabs-active-face ((t (:inherit sr-tabs-inactive-face :foreground "brown1"))))
 '(sr-tabs-inactive-face ((t (:foreground "#fcaf3e" :width extra-expanded))))
 '(variable-pitch ((t (:height 120 :family "Verdana")))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
