;; enable wrap region mode
(wrap-region-global-mode t)

;; begin, end, trigger, mode
(wrap-region-add-wrapper "`" "'")                  ; hit ` then region -> `region'
(wrap-region-add-wrapper "/*" "*/" "/")            ; hit / then region -> /*region*/
(wrap-region-add-wrapper "$" "$" nil 'latex-mode)  ; hit $ then region -> $region$ in latex-mode
