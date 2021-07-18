(defvar aru-window-configuration nil
  "Current window configuration.")

;;;###autoload
(define-minor-mode aru-window
  "Toggle between single and multiple windows. Equivalent to
    maximizing."
  :lighter " -M-"
  :global nil
  (if (one-window-p)
      (when aru-window-configuration
        (set-window-configuration aru-window-configuration))
    (setq aru-window-configuration (current-window-configuration))
    (delete-other-windows)))

(provide 'aru-window)
