;;  -*- lexical-binding: t; -*-
;;; mod-emacs-lisp.el --- Configuration for emacs-lisp

;;; Commentary:
;;; Configuration for emacs-lis;p

;;; Code:

(use-package elisp-def
  :ensure
  :general
  (general-nmap
   :keymaps 'elisp-def-mode-map
   "gd" 'elisp-def)
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-def-mode))

(use-package emacs-lisp-mode
  :general
  (local-leader-def
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "er" 'eval-region
    "ee" 'eval-expression
    "ed" 'eval-defun))

(use-package lispy
  :ensure
  :config
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode))

(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-region)

(provide 'mod-emacs-lisp)
;;; mod-emacs-lisp.el ends here
