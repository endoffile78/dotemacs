(require 'magit)
(require 'git-gutter-fringe+)

(set-face-foreground 'git-gutter-fr+-modified "yellow")
(set-face-foreground 'git-gutter-fr+-added    "green")
(set-face-foreground 'git-gutter-fr+-deleted  "red")

(global-git-gutter+-mode)

(setq magit-last-seen-setup-instructions "1.4.0")

(provide 'init-git)
