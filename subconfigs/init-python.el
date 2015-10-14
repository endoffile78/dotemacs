;;; init-python.el --- Configuration for elpy and jedi

(use-package elpy
  :commands elpy-enable
  :init
  (elpy-enable))

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

(provide 'init-python)
