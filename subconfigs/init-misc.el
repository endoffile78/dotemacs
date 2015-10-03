(use-package ujelly-theme)

(use-package libmpdee)

(use-package server
  :config
  (unless (server-running-p)
	(server-start)))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package vimrc-mode
  :commands vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode)))

(use-package nlinum
  :config
  (global-nlinum-mode 1))

(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)

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
	  indent-tabs-mode t)

(setq-default truncate-lines 1
			  backward-delete-function nil)

(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))

(provide 'init-misc)
