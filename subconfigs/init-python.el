(use-package elpy
  :commands elpy-enable
  :config
  (elpy-enable))

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

(provide 'init-python)
