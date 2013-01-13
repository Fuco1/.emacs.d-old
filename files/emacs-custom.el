(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ange-ftp-ftp-program-name "d:\\progs\\ftp.exe")
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(bmkp-last-as-first-bookmark-file "~\\.emacs.d\\bookmarks")
 '(calc-settings-file "~\\.emacs.d\\my-calc.el")
 '(cua-enable-cua-keys nil)
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (my-tango-dark)))
 '(custom-safe-themes (quote ("18b79c737a005b87246232739c5cd21aab19f8c15f3a822d64b8cbb8dbbc8525" "9eb325147f82880372869ae998bd6c93adb34fc80b1b5cb2976744fb4dc73cb7" "32807962da64511b0529913b8fc19d527b7bc1d32e6dc298066e53e5d67de571" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(desktop-buffer-filter (quote my-desktop-filter))
 '(desktop-save t)
 '(foreground-color "#839496")
 '(global-undo-tree-mode t)
 '(golden-ratio-exclude-buffer-names (quote ("*helm kill-ring*")))
 '(ibuffer-fontification-alist (quote ((10 buffer-read-only font-lock-constant-face) (15 (and buffer-file-name (string-match ibuffer-compressed-file-name-regexp buffer-file-name)) font-lock-doc-face) (20 (string-match "^*" (buffer-name)) font-lock-keyword-face) (25 (and (string-match "^ " (buffer-name)) (null buffer-file-name)) italic) (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face) (35 (memq major-mode (quote (dired-mode sr-mode))) font-lock-function-name-face))))
 '(ibuffer-show-empty-filter-groups t)
 '(ido-ubiquitous-command-exceptions (quote (smex-major-mode-commands beautify-smex ido-goto-symbol grep-read-files highlight-phrase)))
 '(ido-ubiquitous-function-exceptions (quote (hi-lock-read-face-name)))
 '(ido-ubiquitous-mode t)
 '(imenu-auto-rescan t)
 '(ispell-personal-dictionary "~/.emacs.d/.ispell")
 '(ispell-program-name "d:\\progs\\Aspell\\bin\\aspell.exe")
 '(ls-lisp-dirs-first t)
 '(markdown-link-space-sub-char "-")
 '(sp-autoescape-string-quote-if-empty (quote (python-mode)))
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
 '(text-mode-hook (quote (text-mode-hook-identify))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)