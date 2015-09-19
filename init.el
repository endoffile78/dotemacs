(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "/subconfigs"))

(defun sanityinc/time-subtract-millis (b a)
	(* 1000.0 (float-time (time-subtract b a))))

(defun fullscreen (&optional f)
	(interactive)
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(when (display-graphic-p)
	(fullscreen))

(require 'init-company)
(require 'init-evil)
(require 'init-flycheck)
(require 'init-keybindings)
(require 'init-markdown)
(require 'init-misc)
(require 'init-projectile)
(require 'init-helm)
(require 'init-yasnippet)
(require 'init-indentation)
(require 'init-irony)
(require 'init-web)
(require 'init-smartparens)
(require 'init-dired)
(require 'init-gtags)
(require 'init-python)
(require 'init-indentation)
(require 'init-magit)

(add-hook 'after-init-hook
		   (lambda ()
			 (message "init completed in %.2fms"
					  (sanityinc/time-subtract-millis after-init-time before-init-time))))

(provide '.emacs)
