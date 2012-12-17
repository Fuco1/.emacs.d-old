;;; Automatic resizing of Emacs windows to the golden ratio

(golden-ratio-enable)

(defadvice ispell-word (around golden-ratio-temp-disable activate)
  (golden-ratio-disable)
  ad-do-it
  (golden-ratio-enable))
