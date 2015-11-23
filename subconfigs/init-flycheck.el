;;; init-flycheck.el --- Configuration for flycheck and flycheck-irony

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package flycheck-irony
	:init
	(eval-after-load 'flycheck
	  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  :config
  (defhydra hydra-flycheck ()
	("l" flycheck-list-errors "List errors")
	("c" flycheck-check-buffer "Check buffer")
	("n" flycheck-next-error "Next error")
	("p" flycheck-previous-error "Prev error"))
  (global-set-key (kbd "C-c f") 'hydra-flycheck/body))

(provide 'init-flycheck)
