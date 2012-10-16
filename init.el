(server-start)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")

(load "files/defuns")

(load "files/org-mode")

(load "files/tabs")
(load "files/global")
(load "files/keys")
(load "files/ido")

(vendor 'smex)
(vendor 'yasnippet)
(vendor 'haskell-mode)
(vendor 'typopunct)
(vendor 'wc-mode)

;; temp stuff
(defalias 'qrr 'query-replace-regexp)
(defalias 'rs 'replace-string)

(fset 'swap-buffer-to-last-used
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 98 return] 0 "%d")) arg)))
(global-set-key (kbd "C-x C-a") 'swap-buffer-to-last-used)

;; ispell
(setq-default ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.emacs.d/.ispell")
(require 'ispell)
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-<f8>") 'flyspell-mode)

;; TEX
;;(load "/home/xgoljer/lib/emacs/tildify.el")
(require 'tildify)


;; Erlang
;(setq load-path (cons "/home/xgoljer/erlang/erlware-erlware-mode-e6d1c72" load-path))
;(setq erlang-root-dir "/home/xgoljer/erlang/dist")
;(setq exec-path (cons "/home/xgoljer/erlang/dist/bin" exec-path))
;(require 'erlang-start)

;; Prolog
;; AISA
;; (load "/packages/run.64/sicstus-4.0.8/lib/sicstus-4.0.8/emacs/sicstus_emacs_init")
;; NYMFE
;; (load "/packages/run/sicstus/lib/sicstus-3.12.7/emacs/sicstus_emacs_init")
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;; (setq prolog-use-sicstus-sd t)
;; (setq auto-mode-alist (cons '("\\.pl$" . prolog-mode) auto-mode-alist))

(setq load-path (cons "/packages/run.64/sicstus-4.0.8/lib/sicstus-4.0.8/emacs" load-path))
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'sicstus)
(setq auto-mode-alist (cons '("\\.pl$" . prolog-mode) auto-mode-alist))

;; Lisp
;;(setq inferior-lisp-program "/home/xgoljer/lisp/bin/clisp")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cursor-color "#657b83")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(display-time-mode t)
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-nick "Fuco")
 '(erc-prompt ">")
 '(erc-timestamp-format "[%H:%M:%S] ")
 '(erc-timestamp-format-left "[%H:%M:%S] ")
 '(erc-timestamp-format-right " [%H:%M:%S]")
 '(foreground-color "#657b83")
 '(imenu-auto-rescan t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-input ((t (:foreground "cyan"))))
 '(comint-highlight-prompt ((t (:foreground "cyan"))))
 '(erc-action-face ((t (:foreground "green"))) t)
 '(erc-current-nick-face ((t (:foreground "red" :weight bold))) t)
 '(erc-input-face ((t (:foreground "cyan"))) t)
 '(erc-my-nick-face ((t (:foreground "cyan"))) t)
 '(erc-nick-default-face ((t (:foreground "yellow" :weight bold))) t)
 '(erc-prompt-face ((t (:background "Black" :foreground "Grey" :weight bold))) t)
 '(erc-timestamp-face ((t nil)) t)
 '(eshell-prompt ((t (:foreground "SlateBlue2" :weight bold))))
 '(fringe ((((class color) (background light)) (:background "grey95" :foreground "black"))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2" :foreground "black"))))
 '(hl-line ((t (:background "gray12"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(region ((((class color) (min-colors 88) (background light)) (:background "color-56"))))
 '(variable-pitch ((t (:height 120 :family "Verdana")))))
