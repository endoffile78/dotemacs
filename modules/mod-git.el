;;; mod-git.el --- Git configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Git configuration
;;
;;; Code:

(setq version-control t
      vc-follow-symlinks t)

(when (display-graphic-p)
  (use-package git-gutter-fringe+
    :ensure
    :diminish git-gutter+-mode
    :config
    (set-face-foreground 'git-gutter-fr+-modified "yellow")
    (set-face-foreground 'git-gutter-fr+-added    "green")
    (set-face-foreground 'git-gutter-fr+-deleted  "red")
    (global-git-gutter+-mode)))

(use-package magit
  :ensure
  :general
  (leader-def
    "g" '(:ignore t :which-key "Git")
    "gg" 'magit-status
    "gd" 'magit-diff--dwim
    "gb" 'magit-blame
    "gl" 'magit-log
    "gr" 'magit-branch
    "gm" 'magit-merge)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package gitignore-mode
  :ensure)

(provide 'mod-git)
;;; mod-git.el ends here
