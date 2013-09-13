;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC

(setq global-mode-string '("" erc-modified-channels-object))

;; special colors for some people
(setq erc-nick-color-alist '(;; ##latin
                             ("theseb" . "dodger blue")
                             ("godmy" . "indian red")
                             ("fuco" . "SkyBlue1") ;;#8cc4ff
                             ("rhemaxx0s" . "medium sea green")
                             ("tonitrus" . "forest green")
                             ;; #emasc
                             ("wgreenhouse" . "green")
                             ("nicferrier" . "indian red")
                             ("ijp" . "cornflower blue")
                             ("forcer" . "forest green")
                             ("fsbot" . "gray35")
                             ("fsbot`" . "gray35")
                             ("jlf" . "NavajoWhite4")
                             ("taylanub" . "DeepSkyBlue1")
                             ("jordigh" . "orchid1")
                             ))

(defun erc-get-color-for-nick (nick)
  "Gets a color for NICK. If NICK is in erc-nick-color-alist, use
that color, else hash the nick and use a random color from the
pool"
  (or (cdr (assoc (downcase nick) erc-nick-color-alist))
      "#edd400"))

(defun erc-put-color-on-nick ()
  "Modifies the color of nicks according to erc-get-color-for-nick"
  (let (word bounds)
    (save-excursion
      (let ((inhibit-read-only t)) (put-text-property (point-min) (point-max) 'read-only nil))
      (goto-char (point-min))
      (while (or (forward-word 1) (not (eobp)))
        (setq bounds (bounds-of-thing-at-point 'word))
        (when bounds
          (setq word (buffer-substring-no-properties
                      (car bounds) (cdr bounds)))
          (when (or (and (erc-server-buffer-p) (erc-get-server-user word))
                    (and erc-channel-users (erc-get-channel-user word)))
            (put-text-property (car bounds) (cdr bounds)
                               'face (cons 'foreground-color
                                           (erc-get-color-for-nick word)))
            ))))))

(add-hook 'erc-insert-post-hook 'erc-put-color-on-nick)

(defun my-erc-init ()
  (modify-syntax-entry ?\_ "w" nil)
  (modify-syntax-entry ?\- "w" nil)
  (modify-syntax-entry ?` "w" nil))
(add-hook 'erc-mode-hook 'my-erc-init)

(defun my-erc-join-channel ()
  (smartparens-mode -1)
  (show-smartparens-mode -1)
  (visual-line-mode 1)
  (erc-fill-mode -1))
(add-hook 'erc-join-hook 'my-erc-join-channel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RCIRC

(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-full-name
                      channels))))
