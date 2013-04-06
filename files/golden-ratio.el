;;; Automatic resizing of Emacs windows to the golden ratio

(golden-ratio-enable)

(defadvice ispell-word (around golden-ratio-temp-disable activate)
  (golden-ratio-disable)
  ad-do-it
  (golden-ratio-enable))

(defadvice ediff-quit (after golden-ratio-enable activate)
  (golden-ratio-enable))

(defadvice ediff-buffers (after golden-ratio-disable activate)
  (golden-ratio-disable)
  (balance-windows)
  (enlarge-window -10)
  (balance-windows)
  (execute-kbd-macro "?")
  (execute-kbd-macro "|"))
