;;; Snippets make typing fun

(require 'dropdown-list)

(yas-global-mode 1)

(setq yas-snippet-dirs '("~/.emacs.d/vendor/yasnippet/snippets"))

(setq yas-prompt-functions '(yas-ido-prompt))

;; Replace yasnippets's TAB
(add-hook 'yas-minor-mode-hook
          (lambda () (define-key yas-minor-mode-map
                       (kbd "TAB") 'smart-tab))) ; was yas/expand
