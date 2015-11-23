;;; init-web.el --- Settings for html, css, js, and php

(add-hook 'html-mode-hook
		  (lambda ()
			(set (make-local-variable 'sgml-basic-offset) 2)))

(use-package web-mode
  :ensure
  :mode (("\\.html?\\'" . web-mode)
		 ("\\.php?\\'" . web-mode))
  :preface
  (defun my-web-mode-hook ()
	(setq web-mode-markup-indent-offset 2
		  web-mode-css-indent-offset 4
		  web-mode-code-indent-offset 4
		  web-mode-enable-auto-pairing nil
		  web-mode-enable-auto-closing t
		  web-mode-style-padding 2
		  web-mode-script-padding 2
		  web-mode-enable-current-element-highlight t
		  web-mode-enable-block-face t)
	(highlight-symbol-mode 0)
	(rainbow-mode)
	(emmet-mode))
  :config
  (add-hook 'web-mode-hook 'my-web-mode-hook))

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode))

(use-package js2-mode
  :ensure
  :mode ("\\.js$" . js2-mode))

(use-package emmet-mode
  :commands emmet-mode
  :diminish emmet-mode)

(provide 'init-web)
