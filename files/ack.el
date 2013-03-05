(use-package ack-and-a-half
  :pre-init
  (progn
    (defvar f9-ack-prefix-map)
    (define-prefix-command 'f9-ack-prefix-map)
    (bind-key "<f9>" 'f9-ack-prefix-map)

    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same))
  :bind
  (("<f9> <f9>" . ack)
   ("<f9> <f10>" . ack-same)
   ("<f9> <f8>" . ack-find-file)
   ("<f9> <f7>" . ack-find-file-same))
  :config
  (progn
    ;; redefine: fix ack on windows
    (defun ack-and-a-half-run (directory regexp pattern &rest arguments)
      "Run ack in DIRECTORY with ARGUMENTS."
      (let ((default-directory (if directory
                                   (file-name-as-directory (expand-file-name directory))
                                 default-directory)))
        (setq arguments (append ack-and-a-half-arguments
                                (ack-and-a-half-arguments-from-options regexp)
                                arguments
                                (list "--")
                                (list (shell-quote-argument pattern))
                                ;;;; ADDED NEXT LINE
                                (list (concat " < " null-device))
                                ))
        (make-local-variable 'compilation-buffer-name-function)
        (let (compilation-buffer-name-function)
          (setq compilation-buffer-name-function 'ack-buffer-name)
          (compilation-start (mapconcat 'identity (nconc (list ack-and-a-half-executable) arguments) " ")
                             'ack-and-a-half-mode))))

    ;; redefine: add current command to the prompt
    (defsubst ack-and-a-half-read (regexp)
      (let* ((default (ack-and-a-half-default-for-read))
             (type (if regexp "pattern" "literal search"))
             (history-var )
             (prompt  (if default
                          (format "[%s] ack %s (default %s): " this-command type default)
                        (format "[%s] ack %s: " this-command type))))
        (read-string prompt
                     (ack-and-a-half-initial-contents-for-read)
                     (if regexp 'ack-regexp-history 'ack-literal-history)
                     default)))))