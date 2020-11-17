;;; mod-python.el --- description -*- lexical-binding: t; -*-
;;
;;; Code:

(defun python-f5 ()
  "Sends the buffer to a python shell."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(use-package python
  :config
  :bind
  (:map python-mode-map
        ("<f5>" . python-f5)))

(use-package virtualenvwrapper
  :ensure
  :general
  (local-leader-def
    :states 'normal
    :keymaps 'python-mode-map
	"v" '(:ignore t :which-key "virtualenv")
    "va" 'venv-workon
    "vd" 'venv-deactivate)
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package pip-requirements
  :ensure)

(provide 'mod-python)
;;; mod-python.el ends here
