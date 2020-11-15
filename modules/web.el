;;; web.el --- description -*- lexical-binding: t; -*-
;;
;;; Code:

(use-package rainbow-mode
  :ensure
  :diminish rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

(use-package emmet-mode
  :ensure
  :commands emmet-mode
  :diminish emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package web-mode
  :ensure
  :commands web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php?\\'" . web-mode)
         ("\\.aspx?\\'" . web-mode))
  :config
  (defun my-web-mode-hook ()
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 4
          web-mode-code-indent-offset 4
          web-mode-enable-auto-pairing nil
          web-mode-enable-auto-closing t
          web-mode-enable-auto-quoting t
          web-mode-style-padding 2
          web-mode-script-padding 2
          web-mode-enable-current-element-highlight t
          web-mode-enable-block-face t))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

(defun my/start-impatient-mode ()
  (interactive)
  (httpd-start)
  (impatient-mode))

(use-package impatient-mode
  :ensure
  :general
  (local-leader-def
    :states 'normal
    :keymaps '(web-mode-map css-mode-map html-mode-map)
    "i" '(:ignore t :which-key "impatient-mode")
    "is" 'my/start-impatient-mode
    "ik" 'httpd-stop))

(provide 'web)
;;; web.el ends here
