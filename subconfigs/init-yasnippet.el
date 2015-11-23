;;; init-yasnippet.el --- Yasnippet settings

(use-package yasnippet
  :ensure
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (defhydra hydra-yasnippet (:exit t)
	("g" yas-global-mode "Global mode")
	("n" yas-new-snippet "New snippet")
	("r" yas-reload-all "Reload"))
  (global-set-key (kbd "C-c y") 'hydra-yasnippet/body))

(provide 'init-yasnippet)
