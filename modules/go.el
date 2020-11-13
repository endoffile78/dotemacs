;;; go.el -*- lexical-binding: t; -*-
;;;
;;; Code:

(use-package go-mode
  :ensure
  :mode ("\\.go\\'" . go-mode)
  :commands go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook 'subword-mode)
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'go)
;;; go.el ends here
