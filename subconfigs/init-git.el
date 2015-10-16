;;; init-git.el --- Configuration for packages related to git

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :config
  (set-face-foreground 'git-gutter-fr+-modified "yellow")
  (set-face-foreground 'git-gutter-fr+-added    "green")
  (set-face-foreground 'git-gutter-fr+-deleted  "red")
  (global-git-gutter+-mode))

(use-package magit
  :ensure)

(provide 'init-git)
