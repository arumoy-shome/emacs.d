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

(provide 'aru-custom)
