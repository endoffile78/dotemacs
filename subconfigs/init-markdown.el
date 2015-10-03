(use-package markdown-mode
  :commands markdown-mode
  :config
  (setq auto-mode-alist
		(cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist)))

(provide 'init-markdown)
