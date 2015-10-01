(require 'ujelly-theme)

(require 'libmpdee)

(require 'server)
(unless (server-running-p)
	(server-start))

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.5)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(require 'nyan-mode)
(nyan-mode 1)

(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

(require 'nlinum)
(global-nlinum-mode 1)

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

(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "highlight-symbol" '(diminish 'highlight-symbol-mode))
(eval-after-load "git-gutter+" '(diminish 'git-gutter+-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "helm" '(diminish 'helm-mode))
(eval-after-load "emmet-mode" '(diminish 'emmet-mode))
(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))

(provide 'init-misc)
