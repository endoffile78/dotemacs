;;; init-smartparens.el --- Smartparens configuration

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  
  (defun sp-web-mode-is-code-context (id action context)
	(and (eq action 'insert)
		 (not (or (get-text-property (point) 'part-side)
				  (get-text-property (point) 'block-side)))))

  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

  (setq sp-show-pair-delay 0
		sp-show-pair-from-inside t)

  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode)
  (smartparens-global-mode t))

(provide 'init-smartparens)
