;;; mod-yaml.el --- description -*- lexical-binding: t; -*-
;;
;;; Code:

(use-package yaml-mode
  :ensure
  :commands yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(provide 'mod-yaml)
;;; mod-yaml.el ends here
