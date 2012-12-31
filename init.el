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
(require 'parenface)

;; autoloads
(autoload 'calc-same-interface "calc" nil t)

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
(load "files/markdown")
(load "files/tramp")
(load "files/ibuffer")
(load "files/undo-tree")
(load "files/expand-region")
(load "files/emacs-lisp-mode")
(load "files/multi-web-mode")
(load "files/latex-mode")
(load "files/mode-line")

;; vendor
(vendor 'smex)
(vendor 'yasnippet)
(vendor 'typopunct 'typopunct-change-language 'typopunct-mode)
(vendor 'wc-mode)
(vendor 'ace-jump-mode 'ace-jump-mode)
(vendor 'revbufs 'revbufs)
(vendor 'shell-pop 'shell-pop)
(vendor 'golden-ratio)
(vendor 'multiple-cursors)
(vendor 'iy-go-to-char 'iy-go-to-char 'iy-go-to-char-backward)
(vendor 'sunrise-commander)
(vendor 'change-inner 'change-inner 'change-outer)
(vendor 'smart-forward 'smart-forward 'smart-backward 'smart-up 'smart-down)
(vendor 'smartparens)

;; Diminish modeline clutter
(load "files/diminish")

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
 '(blink-matching-paren nil)
 '(bmkp-last-as-first-bookmark-file "~\\.emacs.d\\bookmarks")
 '(calc-settings-file "~\\.emacs.d\\my-calc.el")
 '(column-number-mode t)
 '(cua-enable-cua-keys nil)
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (my-tango-dark)))
 '(custom-safe-themes (quote ("d801114c1e0e46a9d021c67c10390d6b5bd348a1a86373c7c8d66219c685b457" "00559ec92b23a583253b6c250f2a937ea75eaea8841fa451e8c92068a3fcc752" "0bf5028a3bfb479dc32fa2ca965d1539a740fd2a1e69049bbd4aa60f7981731d" "76e2831c55da252d8c916fb249913d21b6bd9f67cfc79d5855ffa1c13e639b48" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
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
 '(golden-ratio-exclude-buffer-names (quote ("*helm kill-ring*")))
 '(ibuffer-fontification-alist (quote ((10 buffer-read-only font-lock-constant-face) (15 (and buffer-file-name (string-match ibuffer-compressed-file-name-regexp buffer-file-name)) font-lock-doc-face) (20 (string-match "^*" (buffer-name)) font-lock-keyword-face) (25 (and (string-match "^ " (buffer-name)) (null buffer-file-name)) italic) (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face) (35 (memq major-mode (quote (dired-mode sr-mode))) font-lock-function-name-face))))
 '(ibuffer-show-empty-filter-groups t)
 '(ido-ubiquitous-command-exceptions (quote (smex-major-mode-commands beautify-smex ido-goto-symbol grep-read-files highlight-phrase)))
 '(ido-ubiquitous-function-exceptions (quote (hi-lock-read-face-name)))
 '(ido-ubiquitous-mode t)
 '(imenu-auto-rescan t)
 '(line-number-mode nil)
 '(ls-lisp-dirs-first t)
 '(menu-bar-mode nil)
 '(sp-autoescape-string-quote-if-empty (quote (python-mode)))
 '(sp-autoinsert-inhibit-functions nil)
 '(sp-autoinsert-quote-if-followed-by-closing-pair nil)
 '(sp-ignore-modes-list (quote (calc-mode dired-mode ibuffer-mode minibuffer-inactive-mode sr-mode help-mode)))
 '(sp-navigate-consider-symbols t)
 '(sr-attributes-display-mask (quote (nil nil nil nil t t t)))
 '(sr-listing-switches "-alh")
 '(sr-popviewer-enabled nil)
 '(sr-show-file-attributes nil)
 '(sr-tabs-max-tabsize 18)
 '(sr-traditional-other-window nil)
 '(sr-windows-locked nil)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
