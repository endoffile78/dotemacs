;;; init-smartparens.el --- Smartparens configuration

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-delay 0
		sp-show-pair-from-inside t)
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode)
  (smartparens-global-mode t))

(provide 'init-smartparens)
