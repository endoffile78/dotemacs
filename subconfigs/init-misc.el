(require 'ujelly-theme)

;(require 'solarized-theme)
;(load-theme 'solarized-dark t)

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

(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

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
	  ;initial-major-mode 'text-mode)

(setq-default truncate-lines 1
			  backward-delete-function nil)

(provide 'init-misc)
