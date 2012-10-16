;; Only activate the mode for text mode
;; (add-hook 'text-mode-hook 'my-text-init)
;; (defun my-text-init ()
;;   (require 'typopunct)
;;   (typopunct-change-language 'english)
;;   (typopunct-mode 1))



(defconst typopunct-minus (decode-char 'ucs #x2212))
(defconst typopunct-pm    (decode-char 'ucs #xB1))
(defconst typopunct-mp    (decode-char 'ucs #x2213))
(defadvice typopunct-insert-typographical-dashes
  (around minus-or-pm activate)
  (cond
   ((or (eq (char-before) typopunct-em-dash)
        (looking-back "\\([[:blank:]]\\|^\\)\\^"))
    (delete-char -1)
    (insert typopunct-minus))
   ((looking-back "[^[:blank:]]\\^")
    (insert typopunct-minus))
   ((looking-back "+/")
    (progn (replace-match "")
           (insert typopunct-pm)))
   (t ad-do-it)))
(defun typopunct-insert-mp (arg)
  (interactive "p")
  (if (and (= 1 arg) (looking-back "-/"))
      (progn (replace-match "")
             (insert typopunct-mp))
    (self-insert-command arg)))
(define-key typopunct-map "+" 'typopunct-insert-mp)



(defconst typopunct-ellipsis (decode-char 'ucs #x2026))
(defconst typopunct-middot   (decode-char 'ucs #xB7)) ; or 2219
(defun typopunct-insert-ellipsis-or-middot (arg)
  "Change three consecutive dots to a typographical ellipsis mark."
  (interactive "p")
  (cond
   ((and (= 1 arg)
         (eq (char-before) ?^))
    (delete-char -1)
    (insert typopunct-middot))
   ((and (= 1 arg)
         (eq this-command last-command)
         (looking-back "\\.\\."))
    (replace-match "")
    (insert typopunct-ellipsis))
   (t
    (self-insert-command arg))))
(define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot)


(defconst typopunct-prime  (decode-char 'ucs #x2032))
(defconst typopunct-dprime (decode-char 'ucs #x2033))
(defconst typopunct-tprime (decode-char 'ucs #x2034))
(defadvice typopunct-insert-quotation-mark (around primes activate)
  (cond
   ((or mark-active
        (not (eq last-command-char ?')))
    ad-do-it)
   ((eq (char-before) ?^)
    (delete-char -1)
    (insert typopunct-prime))
   ((eq (char-before) typopunct-prime)
    (delete-char -1)
    (insert typopunct-dprime))
   ((eq (char-before) typopunct-dprime)
    (delete-char -1)
    (insert typopunct-tprime))
   (t ad-do-it)))


(require 'iso-transl)
(iso-transl-define-keys
 `(("^0" . ,(vector (decode-char 'ucs #x2070)))
   ("^4" . ,(vector (decode-char 'ucs #x2074))) ; 1-3 already defined
   ("^5" . ,(vector (decode-char 'ucs #x2075)))
   ("^6" . ,(vector (decode-char 'ucs #x2076)))
   ("^7" . ,(vector (decode-char 'ucs #x2077)))
   ("^8" . ,(vector (decode-char 'ucs #x2078)))
   ("^9" . ,(vector (decode-char 'ucs #x2079)))
   ("^+" . ,(vector (decode-char 'ucs #x207A)))
   ("^-" . ,(vector (decode-char 'ucs #x207B)))
   ("^=" . ,(vector (decode-char 'ucs #x207C)))
   ("^(" . ,(vector (decode-char 'ucs #x207D)))
   ("^)" . ,(vector (decode-char 'ucs #x207E)))
   ("_0" . ,(vector (decode-char 'ucs #x2080)))
   ("_1" . ,(vector (decode-char 'ucs #x2081)))
   ("_2" . ,(vector (decode-char 'ucs #x2082)))
   ("_3" . ,(vector (decode-char 'ucs #x2083)))
   ("_4" . ,(vector (decode-char 'ucs #x2084)))
   ("_5" . ,(vector (decode-char 'ucs #x2085)))
   ("_6" . ,(vector (decode-char 'ucs #x2086)))
   ("_7" . ,(vector (decode-char 'ucs #x2087)))
   ("_8" . ,(vector (decode-char 'ucs #x2088)))
   ("_9" . ,(vector (decode-char 'ucs #x2089)))
   ("_+" . ,(vector (decode-char 'ucs #x208A)))
   ("_-" . ,(vector (decode-char 'ucs #x208B)))
   ("_=" . ,(vector (decode-char 'ucs #x208C)))
   ("_(" . ,(vector (decode-char 'ucs #x208D)))
   ("_)" . ,(vector (decode-char 'ucs #x208E)))))
