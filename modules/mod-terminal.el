;;; mod-terminal.el --- description -*- lexical-binding: t; -*-
;;
;;; Code:

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (delete-window (selected-window)))

(defun my/open-term ()
  (interactive)
  (let ((w (split-window-below)))
    (select-window w)
    (term shell-file-name))
  (switch-to-buffer "*terminal*"))

(provide 'mod-terminal)
;;; mod-terminal.el ends here
