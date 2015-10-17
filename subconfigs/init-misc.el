;;; init-misc.el --- Misc. settings

(use-package libmpdee)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package vimrc-mode
  :mode (".vim\\(rc\\)?$" . vimrc-mode))

(use-package nlinum
  :config
  (global-nlinum-mode 1))

(setq ring-bell-function 'ignore 
	  browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "firefox"
	  inhibit-startup-message t
	  initial-scratch-message nil
	  auto-save-default nil
	  show-paren-delay 0
	  column-number-mode t
	  make-backup-files t
	  version-control t
	  backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
	  delete-old-versions t
	  vc-follow-symlinks t
	  initial-major-mode 'text-mode)

(setq-default truncate-lines 1
			  backward-delete-function nil)

(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))

(provide 'init-misc)
