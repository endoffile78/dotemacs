;;; mod-javascript.el --- description -*- lexical-binding: t; -*-
;;
;;  description
;;
;;; Code:

(use-package js2-mode
  :ensure
  :commands js2-mode
  :mode ("\\.js$" . js2-mode))

(use-package typescript-mode
  :ensure)

(use-package tide
  :ensure
  :config
  (defun my-tide-hook ()
    (tide-setup)
    (tide-hl-identifier-mode))
  (add-hook 'typescript-mode-hook #'my-tide-hook))

(provide 'mod-javascript)
;;; mod-javascript.el ends here
