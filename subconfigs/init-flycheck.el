(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package flycheck-irony
	:init
	(eval-after-load 'flycheck
	  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
	))

(provide 'init-flycheck)
