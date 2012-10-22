(server-start)

;; Emacs gurus don't need no stinking scroll bars & widgets
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

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
(load "files/defuns")
(load "files/macros")
(load "files/tabs")
(load "files/keys")
(load "files/ido")
(load "files/ispell")

(load "files/undo-tree")


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
(vendor 'key-chord)
(vendor 'wrap-region)

;; TEX
(require 'tildify)

(setq load-path (cons "/packages/run.64/sicstus-4.0.8/lib/sicstus-4.0.8/emacs" load-path))
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'sicstus)
(setq auto-mode-alist (cons '("\\.pl$" . prolog-mode) auto-mode-alist))

;; autoopen files
(find-file "~/.emacs.d/init.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#002b36")
 '(background-mode dark)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(display-time-mode t)
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-nick "Fuco")
 '(erc-prompt ">")
 '(erc-timestamp-format "[%H:%M:%S] ")
 '(erc-timestamp-format-left "[%H:%M:%S] ")
 '(erc-timestamp-format-right " [%H:%M:%S]")
 '(foreground-color "#839496")
 '(global-undo-tree-mode t)
 '(imenu-auto-rescan t)
 '(menu-bar-mode nil)
 '(show-paren-mode t)
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
 '(dropdown-list-face ((t (:inherit default :background "lightyellow" :foreground "black" :family "Consolas"))))
 '(erc-action-face ((t (:foreground "green"))) t)
 '(erc-current-nick-face ((t (:foreground "red" :weight bold))) t)
 '(erc-input-face ((t (:foreground "cyan"))) t)
 '(erc-my-nick-face ((t (:foreground "cyan"))) t)
 '(erc-nick-default-face ((t (:foreground "yellow" :weight bold))) t)
 '(erc-prompt-face ((t (:background "Black" :foreground "Grey" :weight bold))) t)
 '(erc-timestamp-face ((t nil)) t)
 '(eshell-prompt ((t (:foreground "SlateBlue2" :weight bold))) t)
 '(fixed-pitch ((t (:height 100 :family "Consolas"))))
 '(fringe ((((class color) (background light)) (:background "grey95" :foreground "black"))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2" :foreground "black"))))
 '(hl-line ((t (:background "gray12"))) t)
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(org-table ((t (:inherit fixed-pitch :foreground "LightSkyBlue"))))
 '(region ((((class color) (min-colors 88) (background light)) (:background "color-56"))))
 '(variable-pitch ((t (:height 120 :family "Verdana")))))
(put 'narrow-to-region 'disabled nil)
