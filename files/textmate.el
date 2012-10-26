;;This package gives some of the functionality provided by TextMate to
;;Emacs. In particular, the automatic insertion of braces and string
;;delimiters. It is heavily based on work by Orestis Markou
;;(http://code.google.com/p/emacs-textmate/).

(tm/initialize)
(add-to-list 'tm/ignore-modes-list 'sr-mode)
(add-to-list 'tm/ignore-modes-list 'minibuffer-inactive-mode)
(add-to-list 'tm/ignore-modes-list 'ibuffer-mode)

(setq tm/use-open-next-line nil)
(setq skeleton-autowrap nil)
