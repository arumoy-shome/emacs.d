;; used by shell and term
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))

;; used by emacs to find programs (such as grep and find)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Library/TeX/texbin")

(provide 'aru-path)
