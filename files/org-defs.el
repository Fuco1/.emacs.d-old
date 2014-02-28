(use-package org-drill
  :commands org-drill
  :init
  (progn
    (bind-keys :map org-mode-map
      ("H-d" . org-drill)
      ("H-r" . org-drill-resume)
      ("H-a" . org-drill-again)))
  :config
  (progn
    (defun org-drill-present-two-sided-card-no-cloze ()
      (with-hidden-comments
       (let ((drill-sections (org-drill-hide-all-subheadings-except nil)))
         (when drill-sections
           (save-excursion
             (goto-char (nth (random* (min 2 (length drill-sections)))
                             drill-sections))
             (org-show-subtree)))
         (ignore-errors
           (org-display-inline-images t))
         (org-cycle-hide-drawers 'all)
         (prog1 (org-drill-presentation-prompt)
           (org-drill-hide-subheadings-if 'org-drill-entry-p)))))))

(use-package org-agenda
  :defer t
  :config
  (bind-keys :map org-agenda-mode-map
             ("C-n" . org-agenda-next-item)
             ("C-p" . org-agenda-previous-item)
             ("P" . my-org-narrow-to-project)
             ("U" . my-org-narrow-to-parent)
             ("N" . my-org-narrow-to-subtree)
             ("W" . my-org-widen)))

(defface my-org-bold
  '((t (:weight bold :inherit font-lock-variable-name-face)))
  "The face used to highlight pair overlays.")

(defface my-org-italic
  '((t (:slant italic :inherit font-lock-variable-name-face)))
  "The face used to highlight pair overlays.")

(defface my-org-code
  '((t (:family "Consolas" :inherit font-lock-constant-face)))
  "The face used to highlight pair overlays.")

(bind-keys :map org-mode-map
  ("TAB" . smart-tab)
  ("C-e" . my-end-of-code-or-line)
  ("C-a" . my-back-to-indentation-or-beginning)
  ("C-c C-x r" . org-clock-remove-overlays)
  ;; TODO lepsia mapa pre "toggle prikazy?"
  ("C-c C-x L" . org-toggle-link-display)
  ("C-c R" . org-remove-occur-highlights)

  ("C-x n t" . my-org-narrow-to-top-heading)
  ("C-x n P" . my-org-narrow-to-project)
  ("C-x n N" . my-org-narrow-to-subtree)
  ("C-x n W" . my-org-widen))

(defun my-org-open-at-point (&optional arg)
  "Just like `org-open-at-point', but open link in this window."
  (interactive "P")
  (if (equal arg '(16))
      (org-open-at-point arg)
    (let ((current-prefix-argument nil))
      (if arg
          (org-open-at-point '(4))
        (let ((org-link-frame-setup (acons 'file 'find-file org-link-frame-setup)))
          (org-open-at-point '(4)))))))
(bind-key "C-c C-o" 'my-org-open-at-point org-mode-map)
(bind-key "C-c C-=" 'org-open-at-point org-mode-map)

(defun my-goto-current-clocked-task ()
  (interactive)
  (org-goto-marker-or-bmk org-clock-marker))
(bind-key "<f1> <f10>" 'my-goto-current-clocked-task)

(defun my-org-metacontrolreturn ()
  "Execute `org-meta-return' followed by `org-meta-right'.
This usually makes new item indented one level deeper."
  (interactive)
  (org-meta-return)
  (org-metaright))
(bind-key "<C-M-return>" 'my-org-metacontrolreturn)

(require 'org-table)
;; org/orgtbl bindings
(defvar my-org-table-map)
(define-prefix-command 'my-org-table-map)
(bind-key "C-c t" 'my-org-table-map org-mode-map)
(bind-key "C-c t" 'my-org-table-map orgtbl-mode-map)
(defvar my-org-table-insert-map)
(define-prefix-command 'my-org-table-insert-map)
(bind-key "C-c t i" 'my-org-table-insert-map org-mode-map)
(bind-key "C-c t i" 'my-org-table-insert-map orgtbl-mode-map)
(bind-key "C-c t i i" 'orgtbl-insert-radio-table orgtbl-mode-map)
(defvar my-org-table-delete-map)
(define-prefix-command 'my-org-table-delete-map)
(bind-key "C-c t d" 'my-org-table-delete-map org-mode-map)
(bind-key "C-c t d" 'my-org-table-delete-map orgtbl-mode-map)

(let ((bindings '(("C-c t i c" org-table-insert-column)
                  ("C-c t i r" org-table-insert-row)
                  ("C-c t d c" org-table-delete-column)
                  ("C-c t d r" org-table-kill-row)))
      (n 1000))
  (dolist (b bindings)
    (define-key org-mode-map (kbd (car b)) (cadr b))
    (org-defkey orgtbl-mode-map (kbd (car b)) (orgtbl-make-binding (cadr b) n (kbd (car b))))
    (setq n (1+ n))))

(defun my-org-select-cell ()
  "Select the cell in org table the point is in."
  (interactive)
  (let ((b (save-excursion
             (re-search-forward "|")
             (backward-char 1)
             (skip-chars-backward " ")
             (point)))
        (e (save-excursion
             (re-search-backward "|")
             (forward-char 1)
             (skip-chars-forward " ")
             (point))))
    (push-mark b t t)
    (goto-char e)))
(bind-key "C-c t" 'my-org-select-cell org-mode-map)

(defun my-markdown-to-org-link (b e)
  (interactive "r")
  (goto-char b)
  (sp-down-sexp)
  (let ((desc (sp-get (sp--next-thing-selection 0)
                (buffer-substring-no-properties :beg :end)))
        (link (progn
                (sp-beginning-of-sexp 2)
                (sp-get (sp--next-thing-selection 0)
                  (buffer-substring-no-properties :beg :end)))))
    (delete-region b e)
    (insert (format "[[%s][%s]]" link desc))))

(defun my-org-make-numbered-list (beg end)
  (interactive "r")
  (string-rectangle beg end "- ")
  (beginning-of-line)
  (org-call-with-arg 'org-cycle-list-bullet 'previous)
  (org-call-with-arg 'org-cycle-list-bullet 'previous))
(bind-key "C-c 1" 'my-org-make-numbered-list org-mode-map)

;; custom filter bindings
(defvar my-org-filter-map)
(define-prefix-command 'my-org-filter-map)
(bind-key "C-c F" 'my-org-filter-map org-mode-map)

(defmacro my-org-custom-filter (tag key)
  (let ((filter-name (intern (concat "my-org-" (symbol-name tag) "-filter")))
        (filter-name-no-done (intern (concat "my-org-" (symbol-name tag) "-no-done-filter")))
        (filter-name-next (intern (concat "my-org-" (symbol-name tag) "-next-filter")))
        (filter-string (concat "+" (upcase (symbol-name tag))))
        (filter-string-no-done (concat "+" (upcase (symbol-name tag)) "-TODO=\"DONE\""))
        (filter-string-next (concat "+" (upcase (symbol-name tag)) "+TODO=\"NEXT\"")))
    `(progn
      (defun ,filter-name ()
        (interactive)
        (org-match-sparse-tree nil ,filter-string))
      (defun ,filter-name-no-done ()
        (interactive)
        (org-match-sparse-tree nil ,filter-string-no-done))
      (defun ,filter-name-next ()
        (interactive)
        (org-match-sparse-tree nil ,filter-string-next))
      (bind-key ,(concat "C-c F " key) ',filter-name org-mode-map)
      (bind-key ,(concat "C-c F " (upcase key)) ',filter-name-no-done org-mode-map)
      (bind-key ,(concat "C-c F M-" key) ',filter-name-next org-mode-map))))

(my-org-custom-filter books "b")
(my-org-custom-filter mov "m")

(load "files/org-project")

;;;_. Narrowing

(defun my-org-narrow-to-top-heading ()
  "Narrow to the top-most tree containing point."
  (interactive)
  (save-excursion
    (ignore-errors (while (outline-up-heading 1)))
    (org-narrow-to-subtree)))

;; abstract these three into a macro, maybe?
(defun my-org--narrow-to-subtree ()
  "Narrow to subtree at point and also set agenda restriction."
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun my-org-narrow-to-subtree ()
  "Narrow to subtree at point and also set agenda restriction.

If invoked from agenda, narrow to the subtree of the task at
point and rebuild the agenda view."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (my-org--narrow-to-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (my-org--narrow-to-subtree)))

(defun my-org--narrow-to-parent ()
  "Narrow to parent of subtree at point and also set agenda restriction."
  (widen)
  (save-excursion
    (org-up-heading-safe)
    (my-org--narrow-to-subtree)))

(defun my-org-narrow-to-parent ()
  "Narrow to parent of subtree at point and also set agenda restriction.

If invoked from agenda, narrow to the parent of the task at point
and rebuild the agenda view."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (my-org--narrow-to-parent))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (my-org--narrow-to-parent)))

(defun my-org--narrow-to-project ()
  "Narrow to project at point and also set agenda restriction."
  (widen)
  (save-excursion
    (bh/find-project-task)
    (my-org--narrow-to-subtree)))

(defun my-org-narrow-to-project ()
  "Narrow to project at point and also set agenda restriction.

If invoked from agenda, narrow to the project of the task at
point and rebuild the agenda view."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (my-org--narrow-to-project))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (my-org--narrow-to-project)))

(defun my-org-widen ()
  "Remove agenda restrictions or widen the buffer."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)
    (org-agenda-remove-restriction-lock)))

;;;_. Capture
(setq org-directory "~/org")
(setq org-default-notes-file "~/org/refile.org")

;; I use C-M-r to start capture mode
(bind-key "C-M-r" 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n" :clock-keep t)
              ("s" "someday" entry (file "~/org/refile.org")
               "* SOMEDAY %?\n%U\n" :clock-keep t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%" :clock-keep t)
              ("j" "Journal" entry (file+datetree "~/org/diary.org")
               "* %?\n%U\n" :clock-keep t))))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;;;;;;;;;;;;;;;;; REFILING
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)

;;;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

(setq org-log-done 'time)
(setq org-log-done 'note)

;; english locale
(setq system-time-locale "C")
(setq org-completion-use-ido t)

;; support selecting with shift+arrows
(setq org-support-shift-select t)

;; fast state selection
(setq org-use-fast-todo-selection t)

(setq org-agenda-files (quote ("~/org/")))

;; TODO KEYWORDS SETTINGS
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
        (sequence "SOMEDAY(S)" "|")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "IndianRed1" :weight bold)
              ("NEXT" :foreground "RoyalBlue" :weight bold)
              ("DONE" :foreground "LimeGreen" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "orange" :weight bold)
              ("CANCELLED" :foreground "LimeGreen" :weight bold)
              ("SOMEDAY" :foreground "pink" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("SOMEDAY" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Tags shortcuts
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@school" . ?o)
                            ("@home" . ?H)
                            (:endgroup)
                            ("READING" . ?r)
                            ("LATIN" . ?l))))

;;;;;;;;;;;;;;;;;;;;;;;;; AGENDA VIEW
(defun my-org-agenda-is-task-p ()
  "Return non-nil if line at point is a task."
  (org-get-at-bol 'org-marker))

(defun my-org-agenda-remove-empty-lists ()
  (let ((headers '("Tasks to Refile"
                   "Stuck Projects"
                   "Next Tasks"
                   "Tasks"
                   "Projects"
                   "Waiting and Postponed Tasks"
                   "Reading")))
    (let ((case-fold-search nil))
      (--each headers
        (save-excursion
          (goto-char (point-min))
          (search-forward it nil t)
          (unless (save-excursion
                    (forward-line)
                    (my-org-agenda-is-task-p))
            (delete-region (line-beginning-position) (1+ (line-end-position))))))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (regexp-opt headers) nil t)
          (goto-char (match-beginning 0))
          (backward-char)
          (insert (propertize (concat "\n" (make-string (/ (window-width) 2) ?─)) 'face 'org-time-grid)))))))

(add-hook 'org-agenda-finalize-hook 'my-org-agenda-remove-empty-lists)

;; Custom agenda command definitions
(defun my-org-agenda-filter (prefix title &rest args)
  `((,prefix . ,title)
    (,(concat prefix "a") . "All")
    (,(concat prefix "p") . "Ready")
    (,(concat prefix "d") . "Done")
    ,@(--mapcat
       `((,(concat prefix (car it)) tags-todo ,(concat "+" (cdr it) "+TODO=\"NEXT\""))
         (,(concat prefix "a" (car it)) tags ,(concat "+" (cdr it)))
         (,(concat prefix "p" (car it)) tags ,(concat "+" (cdr it) "-TODO=\"DONE\"")
          ((org-agenda-skip-function '(let ((next-headline (save-excursion
                                                             (or (outline-next-heading)
                                                                 (point-max)))))
                                        (when (member "BOOKS" (org-get-tags))
                                          next-headline)))))
         (,(concat prefix "d" (car it)) tags ,(concat "+" (cdr it) "+TODO=\"DONE\"")
          ((org-agenda-cmp-user-defined 'my-org-compare-closed-entries)
           (org-agenda-sorting-strategy '(user-defined-up)))))
       args)))

(setq org-agenda-custom-commands
      `((" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
          (tags-todo "-WAITING-CANCELLED-BOOKS/!NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy '(priority-down todo-state-down effort-up category-keep))))
          (tags-todo "-REFILE-CANCELLED-Reading/!-HOLD-WAITING-SOMEDAY"
                     ((org-agenda-overriding-header "Tasks")
                      (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-HOLD-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELLED+WAITING/!"
                     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                      (org-agenda-skip-function 'bh/skip-stuck-projects)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-todo-ignore-deadlines 'future)))
          (tags-todo "Reading"
                     ((org-agenda-overriding-header "Reading")
                      (org-tags-match-list-sublevels nil))))
         nil)
        ("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
        ,@(my-org-agenda-filter "f" "Media filter" '("b" . "BOOKS") '("m" . "MOV"))))

(defun my-org-compare-closed-entries (a b)
  "Compare two agenda entries A and B based on CLOSED time."
  (let ((closed-a (org-time-string-to-time (org-entry-get (get-text-property 1 'org-marker a) "CLOSED")))
        (closed-b (org-time-string-to-time (org-entry-get (get-text-property 1 'org-marker b) "CLOSED"))))
    (cond
     ((equal closed-a closed-b) nil)
     ((time-less-p closed-a closed-b) -1)
     (t +1))))

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; this is added to the clock-in hook
(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p)
           (not (org-is-habit-p)))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun my-org-add-drill-entry ()
  (interactive)
  (insert
   (format
    "* Word :drill:
    :PROPERTIES:
    :DRILL_CARD_TYPE: twosidednocloze
    :END:
** %s

** English

** Examples

** Notes

"
    my-org-drill-language))
  (re-search-backward ":PROPERTIES:" nil t)
  (org-cycle)
  (re-search-forward ":END:" nil t)
  (forward-line 2))

(defun my-format-russian-verb ()
  (interactive)
  (beginning-of-line)
  (forward-word)
  (kill-sexp)
  (delete-char 1)
  (save-excursion (insert " "))
  (transpose-words 1)
  (backward-word 2)
  (insert "- ")
  (forward-word)
  (insert ":")
  (forward-word)
  (newline)
  (delete-char 2)
  (insert "- ")
  (forward-char 4)
  (kill-word 1)
  (insert ":")
  (end-of-line)
  (delete-char -1))

(defun my-format-meaning ()
  (interactive)
  (cond
    ((equal my-org-drill-language "Russian") (my-format-russian-meaning))
    ((equal my-org-drill-language "Latin") (my-format-latin-meaning))))

(defun my-format-russian-meaning ()
  (interactive)
  (delete-char -1)
  (outline-previous-visible-heading 1)
  (forward-line)
  (delete-char 1)
  (let ((beg (point))
        (end (progn
               (outline-next-visible-heading 1)
               (forward-line -1)
               (point))))
    (my-org-make-numbered-list beg end)))

(defun my-format-latin-meaning ()
  (interactive)
  (let ((end (1- (cdr (bounds-of-thing-at-point 'line)))))
    (ignore-errors
      (while (search-forward ";" end t)
        (delete-char -1)
        (forward-char)
        (newline)))
    (delete-char -1)
    (forward-line -1)
    (let ((end (point))
          (start (progn
                   (outline-next-visible-heading -1)
                   (forward-line)
                   (point))))
      (my-org-make-numbered-list start end))))

(defun my-org-drill-fulltext (word)
  (interactive "sWord: ")
  (widen)
  (occur word)
  (other-window 1)
  (when (re-search-forward (concat "[0-9]:" word) nil t)
    (end-of-line)))

(bind-key "a" 'my-occur-mode-goto-card-occurrence occur-mode-map)
(defun my-occur-mode-goto-card-occurrence ()
  (interactive)
  (occur-mode-goto-occurrence)
  (outline-up-heading 1)
  (org-narrow-to-subtree)
  (org-show-subtree)
  (org-cycle-hide-drawers t))

(defun my-org-status ()
  (cond
   ((not (marker-buffer org-clock-marker))
    "<fc=#d3d7cf>-:--</fc>")
   (t
    (let* ((status (substring-no-properties org-mode-line-string 1
                                            (1- (length org-mode-line-string))))
           (split-status (split-string status " (")))
      (concat "<fc=#8ae234>" (car split-status) "</fc>")))))
