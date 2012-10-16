;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(require 'org-install)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "<f11>") 'org-clock-goto)

(setq org-modules (quote (org-habit)))

(load "files/org-clock")
(load "files/org-project")

;; Enable modules
(setq org-modules (quote (org-bbdb
                          org-bibtex
                          org-crypt
                          org-gnus
                          org-id
                          org-info
                          org-jsinfo
                          org-habit
                          org-inlinetask
                          org-irc
                          org-mew
                          org-mhe
                          org-protocol
                          org-rmail
                          org-vm
                          org-wl
                          org-w3m)))

; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

;;;;;;;;;;;;;;;;;;;;; CAPTURE
(setq org-directory "~/org")
(setq org-default-notes-file "~/org/refile.org")

;; I use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
	  (quote (("t" "todo" entry (file "~/org/refile.org")
			   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
			  ("s" "someday" entry (file "~/org/refile.org")
			   "* SOMEDAY %?\n%U\n%a\n" :clock-in t :clock-resume t)
			  ("n" "note" entry (file "~/org/refile.org")
			   "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
			  ("j" "Journal" entry (file+datetree "~/org/diary.org")
			   "* %?\n%U\n" :clock-in t :clock-resume t))))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
	(beginning-of-line 0)
	(org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;;;;;;;;;;;;;;;;; REFILING
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
								 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
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

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; Archiving settings
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (daynr (string-to-int (format-time-string "%d" (current-time))))
                 (a-month-ago (* 60 60 24 (+ daynr 1)))
                 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                 (this-month (format-time-string "%Y-%m-" (current-time)))
                 (subtree-is-current (save-excursion
                                       (forward-line 1)
                                       (and (< (point) subtree-end)
                                            (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
            (if subtree-is-current
                next-headline ; Has a date in this month or last month, skip it
              nil))  ; available to archive
        (or next-headline (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;; AGENDA VIEW
;; Compact the block agenda view
;; (setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
	  (quote (("N" "Notes" tags "NOTE"
			   ((org-agenda-overriding-header "Notes")
				(org-tags-match-list-sublevels t)))
			  ("h" "Habits" tags-todo "STYLE=\"habit\""
			   ((org-agenda-overriding-header "Habits")
				(org-agenda-sorting-strategy
				 '(todo-state-down effort-up category-keep))))
			  (" " "Agenda"
			   ((agenda "" nil)
				(tags "REFILE"
					  ((org-agenda-overriding-header "Tasks to Refile")
					   (org-tags-match-list-sublevels nil)))
				(tags-todo "-CANCELLED/!"
						   ((org-agenda-overriding-header "Stuck Projects")
							(org-agenda-skip-function 'bh/skip-non-stuck-projects)))
				(tags-todo "-WAITING-CANCELLED/!NEXT"
						   ((org-agenda-overriding-header "Next Tasks")
							(org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
							(org-agenda-todo-ignore-scheduled t)
							(org-agenda-todo-ignore-deadlines t)
							(org-agenda-todo-ignore-with-date t)
							(org-tags-match-list-sublevels t)
							(org-agenda-sorting-strategy
							 '(todo-state-down effort-up category-keep))))
				(tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING-SOMEDAY"
						   ((org-agenda-overriding-header "Tasks")
							(org-agenda-skip-function 'bh/skip-project-tasks-maybe)
							(org-agenda-todo-ignore-scheduled t)
							(org-agenda-todo-ignore-deadlines t)
							(org-agenda-todo-ignore-with-date t)
							(org-agenda-sorting-strategy
							 '(category-keep))))
				(tags-todo "-HOLD-CANCELLED/!"
						   ((org-agenda-overriding-header "Projects")
							(org-agenda-skip-function 'bh/skip-non-projects)
							(org-agenda-sorting-strategy
							 '(category-keep))))
				(tags-todo "-CANCELLED+WAITING/!"
						   ((org-agenda-overriding-header "Waiting and Postponed Tasks")
							(org-agenda-skip-function 'bh/skip-stuck-projects)
							(org-tags-match-list-sublevels nil)
							(org-agenda-todo-ignore-scheduled 'future)
							(org-agenda-todo-ignore-deadlines 'future)))
				(tags "-REFILE/"
					  ((org-agenda-overriding-header "Tasks to Archive")
					   (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
					   (org-tags-match-list-sublevels nil))))
			   nil)
			  ("r" "Tasks to Refile" tags "REFILE"
			   ((org-agenda-overriding-header "Tasks to Refile")
				(org-tags-match-list-sublevels nil)))
			  ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
			   ((org-agenda-overriding-header "Stuck Projects")
				(org-agenda-skip-function 'bh/skip-non-stuck-projects)))
			  ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
			   ((org-agenda-overriding-header "Next Tasks")
				(org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
				(org-agenda-todo-ignore-scheduled t)
				(org-agenda-todo-ignore-deadlines t)
				(org-agenda-todo-ignore-with-date t)
				(org-tags-match-list-sublevels t)
				(org-agenda-sorting-strategy
				 '(todo-state-down effort-up category-keep))))
			  ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
			   ((org-agenda-overriding-header "Tasks")
				(org-agenda-skip-function 'bh/skip-project-tasks-maybe)
				(org-agenda-sorting-strategy
				 '(category-keep))))
			  ("S" "Someday" todo "SOMEDAY"
			   ((org-agenda-overriding-header "Someday")
                (org-tags-match-list-sublevels nil)))
			  ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
			   ((org-agenda-overriding-header "Projects")
				(org-agenda-skip-function 'bh/skip-non-projects)
				(org-agenda-sorting-strategy
				 '(category-keep))))
			  ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
			   ((org-agenda-overriding-header "Waiting and Postponed tasks"))
			   (org-tags-match-list-sublevels nil))
			  ("A" "Tasks to Archive" tags "-REFILE/"
			   ((org-agenda-overriding-header "Tasks to Archive")
				(org-agenda-skip-function 'bh/skip-non-archivable-tasks)
				(org-tags-match-list-sublevels nil))))))
