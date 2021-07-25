(defvar aru-window-configuration nil
  "Current window configuration.")

;;;###autoload
(define-minor-mode aru-window-mode
  "Toggle between single and multiple windows. Equivalent to
    maximizing."
  :lighter " -M-"
  :init-value nil
  :global nil
  (if (one-window-p)
      (when aru-window-configuration
        (set-window-configuration aru-window-configuration))
    (setq aru-window-configuration (current-window-configuration))
    (delete-other-windows)))


(defun aru-window-other-window-dwim ()
  "Wrapper around `other-window' which is aware of
  `aru-window-mode'. When called while `aru-window-mode' is on,
  disable it and go to other window. Else perform standard
  `other-window'."
  (interactive)
  (if (bound-and-true-p aru-window-mode)
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
  "Disable `aru-window-mode' and invoke `other-window'."
  (aru-window-mode -1)
  (other-window -1))

(provide 'aru-window)
