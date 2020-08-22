;; used by shell and term
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/opt/python@3.8/libexec/bin:" (getenv "PATH")))

;; used by emacs to find programs (such as grep and find)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Library/TeX/texbin")
(add-to-list 'exec-path "/usr/local/opt/python@3.8/libexec/bin")

(provide 'aru-path)
