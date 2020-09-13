;; emacs picks up the paths set explicitly in the config file but does
;; not pick up the paths set by libexec, so we add those back ourselves
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))

;; used by emacs to find programs (such as grep and find)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/opt/coreutils/libexec/gnubin")
(add-to-list 'exec-path "/Library/TeX/texbin")
(add-to-list 'exec-path "/usr/local/opt/python@3.8/libexec/bin")
(add-to-list 'exec-path "/usr/local/opt/ruby/bin")
(add-to-list 'exec-path "/usr/local/lib/ruby/gems/2.7.0/bin")
(provide 'aru-path)
