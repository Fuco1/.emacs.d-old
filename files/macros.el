;; insert a footnote to the text. The footnote is in a format
;; [i] Text of the footnote...
;; It will search for the first [i] in the text body and place \footnote{...} there
(fset 'fninsert
   [C-right C-right C-left ?\C-  ?\C-e ?\M-w ?\M-a ?\C-s ?\C-w ?\C-w right ?\M-< ?\C-s ?\C-s ?\C-  ?\C-\M-a delete ?\M-x ?f ?n ?o return])
