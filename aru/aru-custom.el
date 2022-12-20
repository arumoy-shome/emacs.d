;; aru-custom.el -*- lexical-binding: t; -*-
(defun aru-load-theme (theme)
  "Disable all current themes and load a new one."
  ;; below sexp to interactively get theme is from the load-theme source
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun aru-load-theme-auto (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (pcase appearance
    ('light (aru-load-theme 'modus-operandi))
    ('dark (aru-load-theme 'modus-vivendi))))

(provide 'aru-custom)
