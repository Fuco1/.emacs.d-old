(global-set-key (kbd "s-c") 'mc/edit-lines)

;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "s-.") 'mc/mark-next-like-this)
(global-set-key (kbd "s-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "s-?") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "s-/") 'mc/mark-all-like-this)


;; From active region to multiple cursors:
(global-set-key (kbd "s-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "s-a") 'mc/edit-beginnings-of-lines)

;; Rectangular region mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)
