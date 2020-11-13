;;; yaml.el --- description -*- lexical-binding: t; -*-
;;
;;; Code:

(use-package yaml-mode
  :ensure
  :commands yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(provide 'yaml)
;;; yaml.el ends here
