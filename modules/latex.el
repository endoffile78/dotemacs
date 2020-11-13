;;; latex.el --- description -*- lexical-binding: t; -*-
;;
;;; Code:

(use-package tex
  :ensure nil)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'tex-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)

(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'tex-mode-hook 'flyspell-mode)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(provide 'latex)
;;; latex.el ends here
