(when (eq system-type 'windows-nt)
  (custom-set-variables
   '(ispell-program-name "d:\\progs\\Aspell\\bin\\aspell.exe")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-list (quote (("C-k (" "llparenthesis" "" nil) ("C-k )" "rrparenthesis" "" nil) ("C-k k" (lambda nil (interactive) (insert "\\cata{}") (backward-char)) "" nil) (79 "circ" "" nil) (61 "equiv" "" nil) ("C-k -" (lambda nil (interactive) (insert "\\bar{}") (backward-char)) "" nil) ("<right>" "Rightarrow" "" nil) ("<left>" "Leftarrow" "" nil) ("<up>" "Leftrightarrow" "" nil) ("<f1>" "ldots" "" nil))))
 '(TeX-PDF-mode t)
 '(TeX-command-list (quote (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber") ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command") ("Run Zathura" "zathura --fork -s -x \"emacsclient --no-wait +%%{line} %%{input}\" %s.pdf" TeX-run-command nil t :help "Run Zathura PDF viewer"))))
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$") "%(o?)yap %dS -paper a4r -s 0 %d") ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)yap %dS -paper a4 %d") ("^dvi$" ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$") "%(o?)yap %dS -paper a5r -s 0 %d") ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)yap %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)yap %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)yap %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)yap %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)yap %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)yap %dS %d") ("^pdf$" "." "gsview32 -remote %s -raise %o %(outpage)") ("^html?$" "." "netscape %o"))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list (quote (("View with Zathura" ("zathura" (mode-io-correlate " --synctex-forward %n:0:%b") " %s.pdf")) ("View with Sumatra" ("sumatra -reuse-instance" (mode-io-correlate " -forward-search %b %n ") " %o")))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and start") (output-dvi "Yap") (output-pdf "View with Zathura") (output-html "start"))))
 '(ac-auto-show-menu 0.5)
 '(ac-menu-height 15)
 '(ac-quick-help-delay 0.5)
 '(ac-use-fuzzy t)
 '(ack-and-a-half-use-environment nil)
 '(ag-highlight-search t)
 '(allout-prefixed-keybindings (quote (("[(control ?n)]" allout-next-visible-heading) ("[(control ?p)]" allout-previous-visible-heading) ("[(control ?u)]" allout-up-current-level) ("[(control ?f)]" allout-forward-current-level) ("[(control ?b)]" allout-backward-current-level) ("[(control ?a)]" allout-beginning-of-current-entry) ("[(control ?e)]" allout-end-of-entry) ("[(control ?i)]" allout-show-children) ("[(control ?s)]" allout-show-current-subtree) ("[(control ?t)]" allout-toggle-current-subtree-exposure) ("[?h]" allout-hide-current-subtree) ("[(control ?o)]" allout-show-current-entry) ("[?!]" allout-show-all) ("[?x]" allout-toggle-current-subtree-encryption) ("[? ]" allout-open-sibtopic) ("[?.]" allout-open-subtopic) ("[?,]" allout-open-supertopic) ("[?']" allout-shift-in) ("[?>]" allout-shift-in) ("[?<]" allout-shift-out) ("[(control ?m)]" allout-rebullet-topic) ("[?*]" allout-rebullet-current-heading) ("[?#]" allout-number-siblings) ("[(control ?k)]" allout-kill-topic) ("[(meta ?k)]" allout-copy-topic-as-kill) ("[?@]" allout-resolve-xref) ("[?=?c]" allout-copy-exposed-to-buffer) ("[?=?i]" allout-indented-exposed-to-buffer) ("[?=?t]" allout-latexify-exposed) ("[?=?p]" allout-flatten-exposed-to-buffer) ("[(control ?c)]" allout-hide-bodies))))
 '(ange-ftp-ftp-program-name "d:\\progs\\ftp.exe")
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(bind-key-column-widths (quote (20 . 70)))
 '(bind-key-describe-special-forms t)
 '(bjump-window-jump-after-action-hook (quote (golden-ratio)))
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(bmkp-bmenu-commands-file "~/.emacs.d/bookmarks/emacs-bmk-bmenu-commands.el")
 '(bmkp-bmenu-image-bookmark-icon-file nil)
 '(bmkp-bmenu-state-file "~/.emacs.d/bookmarks/emacs-bmk-bmenu-state.el")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks/bookmarks")
 '(bookmark-version-control t)
 '(browse-kill-ring-quit-action (quote save-and-restore))
 '(bs-configurations (quote (("all" nil nil nil nil nil) ("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last) ("files-and-scratch" "^\\*scratch" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last) ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last) ("dired" nil (lambda (b) (with-current-buffer b (eq major-mode (quote dired-mode)))) nil (lambda (b) t) nil))))
 '(calc-settings-file "~/.emacs.d/calc-settings.el")
 '(calc-undo-length 1000)
 '(calendar-latitude 49.2)
 '(calendar-longitude 16.633)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-view-diary-initially-flag t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(completion-ignored-extensions (quote (".cm/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi")))
 '(cua-enable-cua-keys nil)
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (my-tango-dark)))
 '(custom-safe-themes (quote ("c9625a98eea4274e4ce11197658f94ba0525a2053550ce23b5a33acc32789467" "a489662be7dc432d4912d8f8c59f63d6cf8aa283cba3aef2e6440ad563ce885b" "aa08d051adaff4d18f8517738584138d6a5ed17fd2133680f39d9aaadd5da842" "d54f9a57580321787112c32311e66379d9a3d2dc7db56b0e5678067cd604b188" "1630f22aec92f5ccfad35ff528b9b9231deca6d775666d7c9fc76876957ee51c" "2094974c6ba3dcac00d364404f11bcc18711750996e3b622c40e2764d4eaae01" "32a0ef08500036a4a96194f01fafe3b62c347e5d7bda218bac50a34ab92eab5b" "5758fad12d32577dfd86f53be523c16ceed5f6ad307e9967ac16fe82eef6e32a" "ac968ca0978459c43a263ce991d6385d8e59ae2242699c6907125daa6f0a0d20" "18b79c737a005b87246232739c5cd21aab19f8c15f3a822d64b8cbb8dbbc8525" "9eb325147f82880372869ae998bd6c93adb34fc80b1b5cb2976744fb4dc73cb7" "32807962da64511b0529913b8fc19d527b7bc1d32e6dc298066e53e5d67de571" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-unlispify-remove-prefixes t)
 '(custom-unlispify-tag-names nil)
 '(cycbuf-buffer-sort-function (quote cycbuf-sort-by-recency))
 '(dash-enable-fontlock t)
 '(debug-on-error t)
 '(delete-by-moving-to-trash t)
 '(desktop-buffer-filter (quote my-desktop-filter))
 '(desktop-save t)
 '(diary-file "~/org/diary")
 '(dired-dwim-target t)
 '(dired-filter-mark-prefix "\\")
 '(dired-filter-prefix "/")
 '(dired-filter-saved-filters (quote (("media" (extension "ogg" "flv" "mpg" "avi" "mp4" "mp3")) ("elisp" (extension "el" "elc")))))
 '(dired-guess-shell-alist-user (quote (("\\(?:MP[34]\\|avi\\|flv\\|m\\(?:kv\\|p[34g]\\)\\|ogg\\|wmv\\)" "vlc") ("\\.\\(?:djvu\\|p\\(?:df\\|s\\)\\)\\'" "zathura --fork"))))
 '(dired-isearch-filenames t)
 '(dired-listing-switches "-alh")
 '(dired-omit-extensions (quote (".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs" ".log" ".ilg" ".out" ".ind" ".dsc" ".hi" ".synctex.gz")))
 '(dired-open-extensions (quote (("exe" . "wine"))))
 '(dired-open-functions (quote (dired-open-guess-shell-alist dired-open-by-extension dired-open-subdir)))
 '(dired-open-query-before-exit nil)
 '(dired-open-use-nohup t)
 '(display-time-24hr-format t)
 '(display-time-format "%H:%M ")
 '(display-time-string-forms (quote ((if (and (not display-time-format) display-time-day-and-date) (format-time-string "%a %b %e " now) "") (propertize (format-time-string (or display-time-format (if display-time-24hr-format "%H:%M" "%-I:%M%p")) now) (quote help-echo) (format-time-string "%a %b %e, %Y" now)) (if mail (concat " " (propertize display-time-mail-string (quote display) (\` (when (and display-time-use-mail-icon (display-graphic-p)) (\,@ display-time-mail-icon) (\,@ (if (and display-time-mail-face (memq (plist-get (cdr display-time-mail-icon) :type) (quote (pbm xbm)))) (let ((bg (face-attribute display-time-mail-face :background))) (if (stringp bg) (list :background bg))))))) (quote face) display-time-mail-face (quote help-echo) "You have new mail; mouse-2: Read mail" (quote mouse-face) (quote mode-line-highlight) (quote local-map) (make-mode-line-mouse-map (quote mouse-2) read-mail-command))) ""))))
 '(echo-keystrokes 0.1)
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eldoc-eval-preferred-function (quote pp-eval-expression))
 '(eldoc-in-minibuffer-mode t)
 '(emmet-indentation 2)
 '(emmet-preview-default nil)
 '(emms-player-list nil)
 '(enable-recursive-minibuffers t)
 '(erc-autojoin-channels-alist (quote (("chat.freenode.org" "#emacs" "##latin"))))
 '(erc-away-timestamp-format "<%H:%M:%S>")
 '(erc-fill-column 10000)
 '(erc-fill-mode nil)
 '(erc-header-line-format nil)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-modules (quote (autojoin button completion irccontrols keep-place list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(erc-nick "Fuco")
 '(erc-nick-uniquifier "`")
 '(erc-prompt ">")
 '(erc-server "chat.freenode.org")
 '(erc-timestamp-format "[%H:%M:%S] ")
 '(erc-timestamp-only-if-changed-flag nil)
 '(erc-timestamp-use-align-to t)
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude-server-buffer t)
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT" "333" "353")))
 '(erc-track-position-in-mode-line t)
 '(erc-track-showcount t)
 '(erc-track-visibility nil)
 '(eshell-output-filter-functions (quote (eshell-handle-ansi-color eshell-postoutput-scroll-to-bottom eshell-handle-control-codes eshell-handle-ansi-color eshell-watch-for-password-prompt)))
 '(eshell-prompt-function (lambda nil (concat (format-time-string "[%H:%M:%S]:") (abbreviate-file-name (eshell/pwd)) (if (= (user-uid) 0) "># " ">$ "))))
 '(eshell-prompt-regexp "^[^#$
]*>[#$] ")
 '(exec-path (quote ("/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/local/games" "/usr/local/libexec/emacs/24.3/x86_64-unknown-linux-gnu" "/home/matus/bin")))
 '(flx-ido-mode t)
 '(font-latex-math-environments (quote ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "derivation")))
 '(font-latex-quotes (quote auto))
 '(font-latex-user-keyword-classes (quote (("refs" (("tindex" "{") ("sindex" "{") ("index" "[{") ("cref" "{")) (:inherit (font-lock-constant-face)) command))))
 '(foreground-color "#839496")
 '(free-keys-ignored-bindings (quote (("s" . "pnPNjkhl1234567890qwerb") ("A" . "1234567890qwer,.[]=c"))))
 '(free-keys-modifiers (quote ("" "C" "M" "C-M" "A" "H" "s")))
 '(gc-cons-threshold 20000000)
 '(global-flex-isearch-mode t)
 '(global-undo-tree-mode t)
 '(golden-ratio-exclude-buffer-names (quote ("*helm kill-ring*" "*Ediff Control Panel*" " *guide-key*")))
 '(golden-ratio-extra-commands (quote (windmove-left windmove-right windmove-down windmove-up elwm-transpose-window elwm-activate-window)))
 '(golden-ratio-inhibit-functions (quote (my-golden-ratio-inhibit)))
 '(golden-ratio-mode t)
 '(guide-key-mode t)
 '(guide-key/guide-key-sequence (quote ("C-x r" "C-x 4" "C-x j" "C-x p" "C-x n" "A-x" "M-g" "M-s" (calc-mode "V" "v" "k" "a" "u") (dired-mode "/" "*" "C-t" "%" "c" "\\") (ibuffer-mode "/" "*" "%"))))
 '(guide-key/idle-delay 0.6)
 '(guide-key/popup-window-position (quote bottom))
 '(guide-key/recursive-key-sequence-flag t)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-haskell-doc-mode)))
 '(helm-descbinds-mode t)
 '(ibuffer-fontification-alist (quote ((10 buffer-read-only font-lock-constant-face) (15 (and buffer-file-name (string-match ibuffer-compressed-file-name-regexp buffer-file-name)) font-lock-doc-face) (20 (string-match "^*" (buffer-name)) font-lock-keyword-face) (25 (and (string-match "^ " (buffer-name)) (null buffer-file-name)) italic) (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face) (35 (memq major-mode (quote (dired-mode sr-mode))) font-lock-function-name-face))))
 '(ibuffer-saved-filter-groups (quote (("default" ("Org" (mode . org-mode)) ("emacs-config" (or (predicate let ((bfn (buffer-file-name (current-buffer)))) (when bfn (and (string-match-p "\\.emacs\\.d" bfn) (eq major-mode (quote emacs-lisp-mode))))))) ("emacs" (or (mode . emacs-lisp-mode) (mode . lisp-interaction-mode) (mode . inferior-emacs-lisp-mode))) ("TeX" (or (mode . tex-mode) (mode . plain-tex-mode) (mode . latex-mode))) ("Markdown" (or (mode . markdown-mode) (mode . gfm-mode))) ("Web" (or (mode . html-mode) (mode . css-mode) (mode . php-mode) (mode . js-mode))) ("Dired" (mode . dired-mode)) ("Images" (or (mode . image-dired-display-image-mode) (mode . image-dired-thumbnail-mode) (mode . image-mode))) ("Tramp" (or (name . "tramp"))) ("Programming" (or (mode . c-mode) (mode . perl-mode) (mode . python-mode) (mode . cc-mode)))))))
 '(ibuffer-saved-filters (quote (("irc" ((mode . erc-mode))) ("dipl" ((filename . "_dipl"))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-truncate-lines nil)
 '(ido-decorations (quote ("
-> " "" "
   " "
   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history nil)
 '(ido-everywhere t)
 '(ido-max-directory-size 100000)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/.ido.last")
 '(ido-show-dot-for-dired t)
 '(ido-ubiquitous-command-compatibility-exceptions (quote (ibuffer-switch-to-saved-filter-groups)))
 '(ido-ubiquitous-command-exceptions (quote (smex-major-mode-commands beautify-smex ido-goto-symbol grep-read-files diredp-dired-files)))
 '(ido-ubiquitous-enable-compatibility t)
 '(ido-ubiquitous-function-exceptions (quote (diredp-dired-files-interactive-spec)))
 '(ido-ubiquitous-mode t)
 '(ido-use-faces t)
 '(image-dired-cmd-create-temp-image-options "%p -size %wx%h \"%f\" -sample \"%wx%h>\" -strip jpeg:\"%t\"")
 '(image-dired-cmd-create-thumbnail-options "%p -size %wx%h \"%f\" -resize \"%wx%h>\" -strip jpeg:\"%t\"")
 '(imenu-auto-rescan t)
 '(indicate-empty-lines nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/.emacs.d/init.el")
 '(ispell-personal-dictionary "~/.emacs.d/.ispell")
 '(jump-char-forward-key "m")
 '(keyfreq-autosave-mode t)
 '(keyfreq-file "~/.emacs.d/.emacs.keyfreq")
 '(keyfreq-mode t)
 '(legalese-templates (quote ((emacs-lisp-mode (nil ";;; " legalese-file-name " --- " _ "
" "
" ";; Copyright (C) " legalese-year " " legalese-copyright "
" "
" ";; Author: " legalese-author "
" ";; Maintainer: " legalese-author "
" ";; Version: 0.0.1
" ";; Created: 14th February 2014
" ";; Package-requires: ((dash \"2.6.0\"))
" ";; Keywords: " ((legalese-elisp-keyword) str ", ") & -2 "
" "
" @ (quote (legalese-license)) @ "
" ";;; Commentary:
" "
" ";;; Code:
" "
" "
" "(provide '" legalese-file ")
" ";;; " legalese-file-name " ends here
")) (scheme-mode (nil ";;; " legalese-file-name " --- " _ "
" "
" ";; Copyright (C) " legalese-year " " legalese-copyright "
" "
" ";; Author: " legalese-author "
" "
" @ (quote (legalese-license)) @ "
" ";;; Commentary:
" "
" ";;; Code:
" "
")) (default (nil @ legalese-file-name " --- " _ "
" "
" "Copyright (C) " legalese-year " " legalese-copyright "
" "
" "Author: " legalese-author "
" "
" (quote (legalese-license)) @)))))
 '(line-number-mode t)
 '(look-show-subdirs t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity (quote (uid gid)))
 '(magit-diff-refine-hunk (quote all))
 '(make-pointer-invisible t)
 '(markdown-link-space-sub-char "-")
 '(max-lisp-eval-depth 50000)
 '(max-specpdl-size 10000)
 '(message-log-max 10000)
 '(mis-bindings-alist nil)
 '(mis-make-command "make -j2")
 '(mis-recipes-directory "~/.emacs.d/dev/make-it-so/recipes/")
 '(mouse-highlight nil)
 '(multi-web-global-mode nil nil (multi-web-mode))
 '(mweb-default-major-mode (quote html-mode))
 '(org-M-RET-may-split-line nil)
 '(org-agenda-clock-consistency-checks (quote (:max-duration "10:00" :min-duration 0 :max-gap "0:20" :gap-ok-around ("4:00") :default-face ((:foreground "Red")) :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil)))
 '(org-agenda-compact-blocks t)
 '(org-agenda-files (quote ("~/org/refile.org" "~/org/school.org" "~/org/home.org" "~/org/emacs.org" "~/org/me.org")))
 '(org-agenda-include-diary t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-done t)
 '(org-agenda-span (quote day))
 '(org-agenda-start-with-clockreport-mode nil)
 '(org-agenda-sticky t)
 '(org-agenda-tags-column -140)
 '(org-agenda-window-setup (quote current-window))
 '(org-archive-location "%s_archive::* Archived Tasks")
 '(org-clock-history-length 36)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state (quote bh/clock-in-to-next))
 '(org-clock-into-drawer "CLOCK")
 '(org-clock-mode-line-total (quote current))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-when-done nil)
 '(org-clock-persist t)
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
 '(org-cycle-emulate-tab nil)
 '(org-deadline-warning-days 30)
 '(org-default-priority 67)
 '(org-drill-add-random-noise-to-intervals-p t)
 '(org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
 '(org-drill-card-type-alist (quote ((nil org-drill-present-simple-card) ("simple" org-drill-present-simple-card) ("twosided" org-drill-present-two-sided-card nil t) ("multisided" org-drill-present-multi-sided-card nil t) ("hide1cloze" org-drill-present-multicloze-hide1) ("hide2cloze" org-drill-present-multicloze-hide2) ("show1cloze" org-drill-present-multicloze-show1) ("show2cloze" org-drill-present-multicloze-show2) ("multicloze" org-drill-present-multicloze-hide1) ("hidefirst" org-drill-present-multicloze-hide-first) ("hidelast" org-drill-present-multicloze-hide-last) ("hide1_firstmore" org-drill-present-multicloze-hide1-firstmore) ("show1_lastmore" org-drill-present-multicloze-show1-lastmore) ("show1_firstless" org-drill-present-multicloze-show1-firstless) ("conjugate" org-drill-present-verb-conjugation org-drill-show-answer-verb-conjugation) ("decline_noun" org-drill-present-noun-declension org-drill-show-answer-noun-declension) ("spanish_verb" org-drill-present-spanish-verb) ("translate_number" org-drill-present-translate-number) ("twosidednocloze" org-drill-present-two-sided-card-no-cloze nil t))))
 '(org-drill-forgetting-index 50)
 '(org-drill-learn-fraction 0.55)
 '(org-drill-maximum-duration nil)
 '(org-drill-maximum-items-per-session 40)
 '(org-drill-optimal-factor-matrix (quote ((15 (1.3 . 1.3)) (14 (1.1600000000000001 . 1.211) (1.3 . 1.26)) (13 (1.3 . 1.115) (1.26 . 1.346)) (12 (1.1600000000000001 . 1.072) (1.3 . 1.115) (1.4000000000000001 . 1.35)) (11 (1.4000000000000001 . 1.158) (0.98 . 1.029) (1.1600000000000001 . 1.072) (1.3 . 1.115)) (10 (1.4 . 1.4) (1.26 . 1.346) (1.3 . 1.205) (1.1600000000000001 . 1.159)) (9 (0.98 . 1.029) (1.5 . 1.454) (1.4000000000000001 . 1.158) (1.1600000000000001 . 1.072) (1.3 . 1.115)) (8 (1.4 . 1.4) (1.1600000000000001 . 1.072) (1.3 . 1.211) (0.98 . 1.2)) (7 (1.52 . 1.596) (1.26 . 1.346) (1.3800000000000001 . 1.535) (1.4800000000000002 . 1.558) (1.38 . 1.38) (1.4000000000000001 . 1.158) (0.98 . 1.178) (1.24 . 1.327) (1.1600000000000001 . 1.227) (1.3 . 1.276)) (6 (1.34 . 1.378) (1.9400000000000002 . 2.053) (2.08 . 2.135) (1.3399999999999999 . 1.532) (2.14 . 2.119) (1.94 . 1.94) (1.4400000000000002 . 1.519) (1.66 . 1.66) (1.4000000000000001 . 1.158) (2.2800000000000002 . 2.264) (1.2 . 1.473) (1.8 . 1.865) (1.48 . 1.433) (1.9000000000000001 . 1.961) (1.76 . 1.724) (1.6600000000000001 . 1.793) (0.98 . 1.029) (1.0599999999999998 . 1.274) (1.62 . 1.657) (1.52 . 1.596) (1.38 . 1.38) (1.24 . 1.327) (0.76 . 0.986) (1.3 . 1.276) (1.3800000000000001 . 1.535) (1.1600000000000001 . 1.072)) (5 (1.4000000000000001 . 1.158) (1.9999999999999998 . 2.216) (2.7 . 2.7) (1.7200000000000002 . 1.825) (1.02 . 1.237) (1.86 . 1.923) (2.52 . 2.592) (2.8000000000000003 . 2.804) (2.28 . 2.4) (1.38 . 1.38) (2.66 . 2.696) (1.62 . 1.757) (2.42 . 2.493) (1.4800000000000002 . 1.558) (1.1199999999999999 . 1.468) (2.6 . 2.6) (2.46 . 2.5) (2.36 . 2.404) (1.72 . 1.917) (0.98 . 1.112) (2.2199999999999998 . 2.311) (2.2800000000000002 . 2.302) (2.56 . 2.596) (1.9000000000000001 . 2.05) (2.3200000000000003 . 2.305) (1.58 . 1.654) (1.3399999999999999 . 1.532) (1.54 . 1.84) (2.18 . 2.217) (2.04 . 2.132) (2.22 . 2.22) (2.32 . 2.401) (1.94 . 1.94) (1.6199999999999999 . 1.791) (1.0599999999999998 . 1.274) (1.9400000000000002 . 2.053) (1.2 . 1.473) (2.1799999999999997 . 2.309) (1.76 . 1.971) (1.48 . 1.721) (2.08 . 2.135) (1.1600000000000001 . 1.072) (1.24 . 1.327) (1.6600000000000001 . 1.793) (1.3 . 1.115) (1.8 . 1.865) (1.3800000000000001 . 1.535) (1.52 . 1.596)) (4 (1.2000000000000002 . 1.591) (1.54 . 1.717) (2.2 . 2.389) (1.34 . 1.655) (2.52 . 2.588) (2.48 . 2.472) (2.8 . 2.8) (2.1399999999999997 . 2.304) (2.2800000000000002 . 2.396) (1.38 . 1.38) (2.0 . 1.973) (1.62 . 1.895) (1.66 . 1.66) (2.24 . 2.392) (0.98 . 1.029) (2.6599999999999997 . 2.692) (1.26 . 1.346) (2.9 . 2.908) (0.76 . 0.986) (1.9999999999999998 . 2.215) (1.0599999999999998 . 1.274) (1.3399999999999999 . 1.532) (2.8000000000000003 . 2.8) (2.7 . 2.696) (1.1600000000000001 . 1.072) (2.66 . 2.692) (2.42 . 2.492) (2.56 . 2.592) (1.3 . 1.071) (1.7200000000000002 . 1.788) (1.94 . 1.94) (2.32 . 2.4) (2.6 . 2.596) (2.04 . 2.132) (2.36 . 2.404) (2.5 . 2.5) (1.3800000000000001 . 1.535) (1.24 . 1.327) (1.2 . 1.473) (2.46 . 2.496) (2.1799999999999997 . 2.308) (2.2199999999999998 . 2.311) (1.48 . 1.721) (2.18 . 2.217) (1.52 . 1.596) (2.22 . 2.22) (1.9000000000000001 . 2.049) (1.6600000000000001 . 1.793) (2.08 . 2.135) (1.6199999999999999 . 1.791) (2.3200000000000003 . 2.305) (1.8 . 1.865) (1.76 . 1.971) (1.9400000000000002 . 2.053)) (3 (1.2000000000000002 . 1.325) (1.62 . 1.62) (1.44 . 1.819) (1.34 . 1.378) (2.66 . 2.692) (1.66 . 1.66) (1.4000000000000001 . 1.112) (2.52 . 2.588) (1.4800000000000002 . 1.558) (2.38 . 2.368) (2.42 . 2.492) (1.72 . 2.048) (2.14 . 2.304) (2.2800000000000002 . 2.302) (0.98 . 0.989) (1.1600000000000001 . 1.227) (1.2 . 1.473) (1.3 . 1.276) (1.0599999999999998 . 1.274) (1.24 . 1.327) (2.8000000000000003 . 2.8) (1.3399999999999999 . 1.532) (1.3800000000000001 . 1.535) (2.1799999999999997 . 2.308) (1.86 . 1.923) (1.52 . 1.596) (2.1399999999999997 . 2.304) (2.04 . 2.219) (1.6600000000000001 . 1.793) (1.48 . 1.721) (1.94 . 1.94) (1.6199999999999999 . 1.791) (1.8 . 1.865) (2.18 . 2.217) (2.7 . 2.696) (2.6 . 2.596) (2.56 . 2.593) (1.76 . 1.971) (2.5 . 2.5) (2.3200000000000003 . 2.305) (1.9400000000000002 . 2.053) (2.22 . 2.22) (2.36 . 2.404) (2.32 . 2.401) (2.46 . 2.497) (2.2199999999999998 . 2.311) (1.9000000000000001 . 2.049) (2.08 . 2.135)) (2 (1.9999999999999998 . 2.141) (1.58 . 1.654) (1.34 . 1.655) (2.2399999999999998 . 2.288) (1.66 . 1.66) (2.0 . 2.058) (2.42 . 2.409) (1.9599999999999997 . 2.148) (1.86 . 2.046) (2.0999999999999996 . 2.234) (1.62 . 1.657) (1.5799999999999998 . 1.754) (1.8199999999999998 . 2.126) (2.28 . 2.327) (2.56 . 2.592) (1.38 . 1.38) (1.1600000000000001 . 1.072) (1.2 . 1.473) (1.26 . 1.65) (1.0599999999999998 . 1.274) (1.3 . 1.115) (0.98 . 1.178) (1.24 . 1.327) (1.1199999999999999 . 1.468) (1.6800000000000002 . 1.964) (2.32 . 2.32) (1.3800000000000001 . 1.535) (1.52 . 1.596) (1.3399999999999999 . 1.532) (1.6600000000000001 . 1.793) (1.94 . 1.94) (2.2800000000000002 . 2.302) (2.3200000000000003 . 2.305) (1.48 . 1.721) (1.6199999999999999 . 1.791) (2.22 . 2.22) (1.8 . 1.865) (1.4 . 1.736) (2.18 . 2.217) (1.9400000000000002 . 2.053) (1.76 . 1.971) (2.08 . 2.135) (1.9000000000000001 . 2.049) (2.7 . 2.696) (2.1799999999999997 . 2.231) (2.5 . 2.5) (2.46 . 2.497) (2.2199999999999998 . 2.311) (2.04 . 2.219) (2.6 . 2.596) (2.36 . 2.404)) (1 (1.7200000000000002 . 3.846) (1.2000000000000002 . 3.846) (1.4800000000000002 . 3.282) (2.38 . 3.846) (1.62 . 3.413) (1.94 . 4.0) (2.1399999999999997 . 3.556) (2.28 . 3.698) (0.98 . 3.55) (1.8199999999999998 . 3.402) (2.42 . 3.846) (2.46 . 3.994) (1.66 . 4.0) (1.38 . 4.0) (2.3200000000000003 . 4.154) (1.1600000000000001 . 3.698) (1.2 . 3.55) (1.3 . 3.846) (1.24 . 3.846) (1.0599999999999998 . 3.692) (1.6800000000000002 . 3.538) (1.3800000000000001 . 3.698) (2.22 . 4.0) (2.32 . 3.84) (1.48 . 3.55) (1.52 . 3.846) (1.3399999999999999 . 3.692) (1.6600000000000001 . 3.698) (1.6199999999999999 . 3.692) (1.8 . 3.846) (1.76 . 3.55) (1.9400000000000002 . 3.698) (1.9000000000000001 . 3.692) (2.08 . 3.846) (2.04 . 3.846) (2.2199999999999998 . 3.698) (2.6 . 4.154) (2.1799999999999997 . 3.692) (2.5 . 4.0) (2.36 . 3.846) (1.7000000000000002 . 3.44) (1.96 . 3.538)))))
 '(org-drill-save-buffers-after-drill-sessions-p nil)
 '(org-drill-scope (quote directory))
 '(org-emphasis-alist (quote (("*" my-org-bold "<b>" "</b>") ("/" my-org-italic "<i>" "</i>") ("_" underline "<span style=\"text-decoration:underline;\">" "</span>") ("=" my-org-code "<code>" "</code>" verbatim) ("~" my-org-code "<code>" "</code>" verbatim) ("+" (:strike-through t) "<del>" "</del>"))))
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "zathura --fork %s"))))
 '(org-habit-following-days 1)
 '(org-habit-graph-column 80)
 '(org-habit-preceding-days 30)
 '(org-inline-image-resolve-url (quote (org-inline-image--regexp-resolver identity)))
 '(org-log-done (quote note))
 '(org-log-into-drawer t)
 '(org-lowest-priority 69)
 '(org-modules (quote (org-bbdb org-bibtex org-crypt org-gnus org-habit org-id org-info org-drill)))
 '(org-priority-start-cycle-with-default nil)
 '(org-refile-use-outline-path (quote file))
 '(org-special-ctrl-a/e t)
 '(org-src-fontify-natively t)
 '(org-stuck-projects (quote ("" nil nil "")))
 '(org-tags-exclude-from-inheritance (quote ("folder")))
 '(org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(pj-line-width 1100)
 '(predictive-add-to-dict-ask nil)
 '(predictive-auto-add-to-dict t)
 '(predictive-auto-learn t)
 '(predictive-use-auto-learn-cache nil)
 '(preview-scale-function 1.5)
 '(projectile-global-mode t)
 '(projectile-globally-ignored-directories (quote (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "elpa")))
 '(projectile-project-root-files (quote (".dir-locals.el" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "rebar.config" "project.clj" "pom.xml" "build.sbt" "Gemfile" "Makefile")))
 '(rcirc-fill-column (quote frame-width))
 '(rcirc-server-alist (quote (("dasnet.cz" :port 7001 :password "polakmajstersveta" nil nil))))
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(reftex-label-alist (quote (("lemma" 32 "lem:" "~\\cref{%s}" nil nil) ("theorem" 32 "th:" "~\\cref{%s}" nil nil))))
 '(safe-local-variable-values (quote ((eval font-lock-add-keywords nil (quote (("defexamples\\|def-example-group\\| => " (0 (quote font-lock-keyword-face))) ("(defexamples[[:blank:]]+\\(.*\\)" (1 (quote font-lock-function-name-face)))))) (eval font-lock-add-keywords nil (quote (("defexamples\\|def-example-group\\| => " (0 (quote font-lock-keyword-face))) ("(defexamples[[:blank:]]+\\(.*?\\)" (1 (quote font-lock-function-name-face)))))) (eval font-lock-add-keywords nil (quote (("defexamples\\|def-example-group\\| => " (0 (quote font-lock-keyword-face))) ("(defexamples[:blank:]+\\(.*?\\)" (1 (quote font-lock-function-name-face)))))) (eval font-lock-add-keywords nil (quote (("defexamples\\|def-example-group\\| => " (0 (quote font-lock-keyword-face))) ("(defexamples[ ]+\\(.*?\\)" (1 (quote font-lock-function-name-face)))))) (eval font-lock-add-keywords nil (quote (("defexamples\\|def-example-group\\| => " (0 (quote font-lock-keyword-face))) ("(defexamples +\\(.*?\\)" (1 (quote font-lock-function-name-face)))))) (eval font-lock-add-keywords nil (quote (("defexamples\\|def-example-group\\| => " (0 (quote font-lock-keyword-face)))))) (org-refile-targets ("~/org/bookmarks.org" :tag . "folder")) (org-tags-exclude-from-inheritance "folder") (org-tags-exclude-from-inheritance . folder) (org-tags-exclude-from-inheritance . "folder") (eval progn (variable-pitch-mode 1) (text-scale-adjust 2) (overlay-put (make-overlay (point-min) (point-max)) (quote face) (quote my-reading-face))) (dired-filter-stack (dot-files) (omit)) (my-inhibit-buffer-cleanup . t) (eval progn (local-set-key (kbd "C-=") (quote my-org-add-drill-entry)) (local-set-key (kbd "C-<") (quote my-format-russian-verb)) (local-set-key (kbd "C->") (quote my-format-meaning))) (eval progn (local-set-key (kbd "C-=") (quote my-org-add-drill-entry)) (local-set-key (kbd "C->") (quote my-format-meaning))) (eval progn (local-set-key (kbd "C-=") (quote my-org-add-drill-entry)) (local-set-key (kbd "C->") (quote my-format-latin-meaning))) (eval progn (local-set-key (kbd "C-=") (quote my-org-add-drill-entry)) (local-set-key (kbd "C-<") (quote my-format-russian-verb)) (local-set-key (kbd "C->") (quote my-format-russian-meaning))) (eval progn (variable-pitch-mode 1) (text-scale-adjust 2)) (cursor-type . bar) (eval progn (variable-pitch-mode 1) (text-scale-adjust 3)) (my-org-drill-language . "Latin") (eval local-set-key (kbd "C-=") (quote my-org-add-drill-entry)) (eval set-input-method "cyrillic-translit") (my-org-drill-language . "Russian") (my-org-drill-file . t) (my-org-drill-local-language . "Polish") (eval virtual-dired "d:/") (eval font-lock-add-keywords nil (\` (((\, (concat "(" (regexp-opt (quote ("wd-cond")) t) "\\_>")) 1 (quote font-lock-keyword-face))))) (eval push (file-name-directory (buffer-file-name)) load-path) (eval font-lock-add-keywords nil (\` (((\, (concat "(" (regexp-opt (quote ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")) t) "\\_>")) 1 (quote font-lock-variable-name-face))))) (TeX-master . main) (eval font-lock-add-keywords nil (quote (("(\\(dm-defun\\)\\(?:\\s-\\)+\\(\\_<.*?\\_>\\)" (1 font-lock-keyword-face) (2 font-lock-function-name-face))))) (eval font-lock-add-keywords nil (quote (("defexamples\\| => " (0 (quote font-lock-keyword-face)))))) (reftex-default-bibliography "./bibliography") (eval allout-mode t))))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/.emacs-places")
 '(send-mail-function (quote mailclient-send-it))
 '(shell-pop-autocd-to-working-dir t)
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "<f11>")
 '(shell-pop-window-height 50)
 '(show-smartparens-global-mode t)
 '(smartparens-global-mode t)
 '(smartparens-global-strict-mode t)
 '(smex-save-file "~/.emacs.d/.smex-items")
 '(sp-autoescape-string-quote-if-empty (quote (python-mode)))
 '(sp-autoinsert-if-followed-by-same 3)
 '(sp-autoinsert-quote-if-followed-by-closing-pair nil)
 '(sp-autoskip-closing-pair (quote always))
 '(sp-autoskip-opening-pair nil)
 '(sp-autowrap-region t)
 '(sp-comment-string (quote (((emacs-lisp-mode) . ";; "))))
 '(sp-hybrid-kill-excessive-whitespace nil)
 '(sp-ignore-modes-list (quote (image-dired-display-image-mode image-dired-thumbnail-mode ediff-mode recentf-dialog-mode google-maps-static-mode ibuffer-mode org-agenda-mode dired-mode)))
 '(sp-navigate-close-if-unbalanced t)
 '(sp-navigate-comments-as-sexps t)
 '(sp-navigate-consider-sgml-tags (quote (html-mode markdown-mode gfm-mode rst-mode)))
 '(sp-navigate-consider-stringlike-sexp (quote (ruby-mode gfm-mode emacs-lisp-mode html-mode org-mode python-mode text-mode)))
 '(sp-navigate-consider-symbols t)
 '(sp-navigate-reindent-after-up (quote ((interactive emacs-lisp-mode))))
 '(sp-navigate-skip-match (quote (((ruby-mode enh-ruby-mode) . sp--ruby-skip-match) ((emacs-lisp-mode inferior-emacs-lisp-mode lisp-interaction-mode scheme-mode inferior-scheme-mode geiser-repl-mode lisp-mode eshell-mode slime-repl-mode cider-repl-mode nrepl-repl-mode clojure-mode common-lisp-mode) . sp--elisp-skip-match))))
 '(sp-override-key-bindings nil)
 '(sp-sexp-prefix (quote ((emacs-lisp-mode syntax ".'") (latex-mode syntax "\\"))))
 '(sp-show-enclosing-pair-commands (quote (sp-show-enclosing-pair sp-forward-slurp-sexp sp-backward-slurp-sexp sp-forward-barf-sexp sp-backward-barf-sexp)))
 '(sp-show-pair-from-inside nil)
 '(sp-successive-kill-preserve-whitespace 2)
 '(sp-test-customize (quote ((interactive emacs-lisp-mode))))
 '(sp-undo-pairs-separately t)
 '(sp-wrap-deactivate-smart-symbol-wrapping nil)
 '(sp-wrap-from-point nil)
 '(split-width-threshold 360)
 '(sr-attributes-display-mask (quote (nil nil nil nil t t t)))
 '(sr-listing-switches "-alh")
 '(sr-popviewer-enabled nil)
 '(sr-show-file-attributes nil)
 '(sr-tabs-max-tabsize 18)
 '(sr-traditional-other-window nil)
 '(sr-windows-locked nil)
 '(texmathp-tex-commands (quote (("derivation" env-on))))
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-strip-common-suffix nil)
 '(use-package-verbose t)
 '(user-full-name "Matúš Goljer")
 '(user-mail-address "matus.goljer@gmail.com")
 '(vc-make-backup-files t)
 '(visible-bell nil)
 '(w3m-command nil)
 '(w3m-imagick-convert-program "c:\\cygwin\\bin\\convert.exe")
 '(whitaker-program "(cd /home/matus/languages/Latin/Words/ && wine words.exe)")
 '(winner-mode t)
 '(x-select-enable-clipboard t))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-local-directory ((t (:inherit dired-directory))))
 '(cycbuf-current-face ((t (:inherit hl-line))) t)
 '(cycbuf-header-face ((t (:inherit font-lock-type-face))) t)
 '(diredp-dir-priv ((t (:inherit dired-directory))))
 '(diredp-exec-priv ((t nil)))
 '(diredp-file-name ((t (:inherit default))))
 '(diredp-ignored-file-name ((t (:inherit shadow))))
 '(diredp-link-priv ((t nil)))
 '(diredp-no-priv ((t nil)))
 '(diredp-number ((t (:inherit default))))
 '(diredp-other-priv ((t nil)))
 '(diredp-rare-priv ((t nil)))
 '(diredp-read-priv ((t nil)))
 '(diredp-write-priv ((t nil)))
 '(eldoc-highlight-function-argument ((t (:inherit bold :foreground "#4e9a06"))))
 '(erc-nick-default-face ((t (:inherit erc-default))) t)
 '(eshell-prompt ((t (:foreground "#73d216" :weight normal))) t)
 '(eyebrowse-mode-line-active ((t (:inherit mode-line))) t)
 '(eyebrowse-mode-line-inactive ((t (:inherit mode-line-secondary))) t)
 '(fixed-pitch ((t (:family "Inconsolata"))))
 '(font-latex-sedate-face ((t (:inherit font-lock-keyword-face))))
 '(guide-key/key-face ((t (:inherit font-lock-keyword-face))))
 '(header-line ((t (:inherit mode-line))))
 '(italic ((t (:slant italic))))
 '(my-diredp-media-face ((t (:foreground "#ce5c00"))) t)
 '(my-diredp-sourcefile-face ((t (:foreground "#fcaf3e"))) t)
 '(my-hide-prefix ((t (:underline "#888a85"))) t)
 '(my-space-2 ((t (:inherit default :height 0.21))) t)
 '(my-space-px-4 ((t (:inherit default :height 2.0 :width ultra-condensed))) t)
 '(org-block ((t (:inherit shadow :family "Consolas"))))
 '(org-block-background ((t (:inherit fixed-pitch :background "#232a2b"))))
 '(org-formula ((t (:inherit fixed-pitch :foreground "chocolate1"))))
 '(org-mode-line-clock ((t nil)) t)
 '(org-table ((t (:inherit fixed-pitch :foreground "#8cc4ff"))))
 '(org-verbatim ((t (:inherit org-code))))
 '(sp-pair-overlay-face ((t (:background "#004a5d"))))
 '(sp-show-pair-enclosing ((t (:background "#004a5d"))))
 '(variable-pitch ((t (:weight normal :height 144 :family "CMU Bright"))))
 '(w3m-anchor ((t (:foreground "#729fcf"))) t)
 '(w3m-arrived-anchor ((t (:inherit font-lock-builtin-face))) t)
 '(wgrep-delete-face ((t (:inherit font-lock-warning-face)))))
