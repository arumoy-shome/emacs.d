;; aru-packages.el   -*- lexical-binding: t; -*-

;; init straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; init use-package we have to use straight-use-package since
;; use-package is not loaded yet
(straight-use-package 'use-package)

(setq use-package-compute-statistics t
      use-package-verbose t)

(provide 'aru-packages)
