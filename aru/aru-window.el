(defun aru-window-other-window-dwim ()
  "Wrapper around `other-window'. When called while only one window
  is visible, call `winner-undo' and go to `other-window'. Else
  perform standard `other-window'."
  (interactive)
  (if (one-window-p)
      (aru-window--other-window)
    (other-window -1)))

(defun aru-window-split-window-right ()
  "Wrapper around `split-window-right'. Split the window, switch
  to the new window and balance them."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun aru-window-split-window-below ()
  "Wrapper around `split-window-below'. Split the window, switch
  to the new window and balance them."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun aru-window--other-window ()
  "Call `winner-undo' followed by `other-window'."
  (winner-undo)
  (other-window -1))

(provide 'aru-window)
