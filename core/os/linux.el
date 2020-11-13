;;; linux.el -*- lexical-binding: t; -*-
;;
;;; Code:

(if (file-exists-p "~/bin")
    (add-to-list 'exec-path "~/bin" t))
(add-to-list 'exec-path "/usr/local/bin" t)

(setq-default shell-file-name "/bin/zsh")

(provide 'linux)
;;; linux.el ends here
