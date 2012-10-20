;;; ispell - Interactive spellchecker

(setq-default ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.emacs.d/.ispell")
(require 'ispell)
(global-set-key (kbd "<f10>") 'ispell-word)
(global-set-key (kbd "C-<f10>") 'flyspell-mode)
