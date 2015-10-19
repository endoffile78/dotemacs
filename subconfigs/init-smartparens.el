;;; init-smartparens.el --- Smartparens configuration

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (sp-use-smartparens-bindings)
  (smartparens-global-mode t))

(provide 'init-smartparens)
