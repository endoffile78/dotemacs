;;; init-helm --- Configuration for helm

(use-package helm
  :ensure
  :diminish helm-mode
  :bind (("M-x" . helm-M-x))
  :init
  (setq helm-command-prefix-key "C-c h"
		helm-quick-update t
		helm-bookmark-show-location t
		helm-M-x-fuzzy-match t
		helm-buffers-fuzzy-matching t)
  :config
  (use-package helm-flx
	:config
	(helm-flx-mode +1))
  (use-package helm-projectile
	:config
	(helm-projectile-on))
  (helm-mode 1))

(provide 'init-helm)
