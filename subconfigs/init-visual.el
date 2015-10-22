;;; init-visual.el --- Visual settings

(use-package ujelly-theme
  :load-path "themes/ujelly")

(use-package nyan-mode
  :if window-system
  :config
  (nyan-mode 1))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol-mode
  :init
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package rainbow-delimeters
  :commands rainbow-delimeters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(when window-system
  (show-paren-mode 1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode 1))

(provide 'init-visual)
