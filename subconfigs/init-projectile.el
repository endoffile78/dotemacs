;;; init-projectile.el --- Projectile settings

(use-package projectile
  :config
  (setq projectile-indexing-method 'native
		projectile-enable-caching t)
  (projectile-global-mode))

(provide 'init-projectile)
