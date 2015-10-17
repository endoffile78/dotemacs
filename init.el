(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path (expand-file-name "subconfigs" user-emacs-directory))

(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defun fullscreen (&optional f)
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
						 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
						 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(when (display-graphic-p) ;When in the gui make emacs fullscreen
  (fullscreen))

(unless '(packge-installed-p 'use-package) ;Make sure use-package is installed
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'diminish)
(require 'bind-key)

(use-package init-company)
(use-package init-dired)
(use-package init-evil)
(use-package init-flycheck)
(use-package init-git)
(use-package init-gtags)
(use-package init-helm)
(use-package init-indentation)
(use-package init-irony)
(use-package init-java)
(use-package init-markdown)
(use-package init-misc)
(use-package init-projectile)
(use-package init-python)
(use-package init-smartparens)
(use-package init-visual)
(use-package init-web)
(use-package init-yasnippet)

(add-hook 'after-init-hook
		  (lambda ()
			(message "init completed in %.2fms"
					 (sanityinc/time-subtract-millis after-init-time before-init-time))))

(provide 'init)
