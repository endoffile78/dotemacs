;;; init-projectile.el --- Projectile settings

(defun my-projectile-hook ()
  "Checks to see if project is in a git repo or not and then sets the indexing method"
  (let ((vcs (projectile-project-vcs)))
    (cond
     ((eq vcs 'git) (setq projectile-indexing-method 'alien
						  projectile-enable-caching nil))
     (t (setq projectile-indexing-method 'native
  			  projectile-enable-caching t)))))

(use-package projectile
  :config
  (add-hook 'projectile-before-switch-project-hook 'my-projectile-hook)
  (projectile-global-mode))

(provide 'init-projectile)
