;;; ispell - Interactive spellchecker

;;(require 'ispell)
(autoload 'ispell-word "ispell" nil t)
(global-set-key (kbd "<f10>") 'ispell-word)
(global-set-key (kbd "C-<f10>") 'flyspell-mode)
