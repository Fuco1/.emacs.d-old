;;; Snippets make typing fun

(require 'dropdown-list)

(yas-global-mode 1)

(setq yas-snippet-dirs '("~/.emacs.d/vendor/yasnippet/snippets"))

(setq yas-prompt-functions '(yas-ido-prompt))

(defun my-yas-startup ()
  ;; stupid yasnippet :/ we define <tab> behaviour elsewhere
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))

;; Replace yasnippets's TAB
(add-hook 'yas-minor-mode-hook 'my-yas-startup) ; was yas/expand
