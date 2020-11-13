;;; git.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/endoffile>
;; Maintainer:  <endoffile@localhost>
;; Created: October 24, 2020
;; Modified: October 24, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/endoffile/git
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
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

(use-package evil-magit
  :ensure
  :config
  (evil-magit-init))

(use-package gitignore-mode
  :ensure)

(provide 'git)
;;; git.el ends here
