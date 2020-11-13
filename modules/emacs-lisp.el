;;  -*- lexical-binding: t; -*-
;;; c.el --- Configuration for emacs-lisp

;;; Commentary:
;;; Configuration for emacs-lis;p

;;; Code:

(use-package emacs-lisp-mode
  :general
  (local-leader-def
    :states 'normal
    :keymaps 'emacs-lisp-mode
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

(provide 'emacs-lisp)
;;; emacs-lisp.el ends here
