;; used by shell and term
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/opt/coreutils/libexec/gnubin:" (getenv "PATH")))
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))

;; used by emacs to find programs (such as grep and find)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/opt/coreutils/libexec/gnubin")
(add-to-list 'exec-path "/Library/TeX/texbin")

;; set default directory to $HOME
(setq default-directory (getenv "HOME"))
(provide 'aru-path)
