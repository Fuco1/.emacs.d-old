;;; ispell - Interactive spellchecker

(autoload 'ispell-word "ispell" nil t)
(bind-key "<f10>" 'ispell-word)
(bind-key "C-<f10>" 'flyspell-mode)
