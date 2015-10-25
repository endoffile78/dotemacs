;;; init-python.el --- Configuration for elpy and jedi

(use-package elpy
  :commands elpy-enable
  :bind (("C-c e d" . elpy-goto-definition)
		 ("C-c e s" . elpy-rgrep-symbol)
		 ("C-c e m" . elpy-multiedit))
  :init
  (setq elpy-rpc-backend "jedi")
  (elpy-enable))

(use-package jedi
  :commands jedi:setup
  :init
  (add-hook 'python-mode-hook 'jedi:setup))

(provide 'init-python)
