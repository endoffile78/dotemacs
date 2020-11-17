;;; mod-markdown.el --- description -*- lexical-binding: t; -*-
;;
;;; Code:

(use-package markdown-mode
  :ensure
  :commands markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook 'my/set-fill-column)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-ispell company-files)))))


(provide 'mod-markdown)
;;; mod-markdown.el ends here
