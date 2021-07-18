(defun aru-tab-bar-select-tab-dwim ()
  "Taken from protesilaos. Do-What-I-Mean function for getting to a
`tab-bar-mode' tab. If no other tab exists, create one and switch to
it. If there is one other tab (so two in total) switch to it without
further questions. Else use completion to select the tab to switch
to."
  (interactive)
  (let ((tabs (mapcar (lambda (tab)
                        (alist-get 'name tab))
                      (tab-bar--tabs-recent))))
    (cond ((eq tabs nil)
           (tab-new))
          ((eq (length tabs) 1)
           (tab-next))
          (t (tab-bar-switch-to-tab tabs)))))

(provide 'aru-tab-bar)
