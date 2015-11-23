;;; init-python.el --- Configuration for elpy and jedi

(use-package elpy
  :commands elpy-enable
  :init
  (setq elpy-rpc-backend "jedi")
  (elpy-enable)
  :config
  (defhydra hydra-elpy (:exit t)
	("d" elyp-goto-definition "Got definition")
	("r" elpy-refactor "Refactor")
	("s" elpy-rgrep-symbol "Find symbol")
	("m" elpy-multiedit "Multiple cursors"))
  (global-set-key (kbd "C-c e") 'hydra-elpy/body))

(use-package jedi
  :commands jedi:setup
  :init
  (add-hook 'python-mode-hook 'jedi:setup))

(provide 'init-python)
