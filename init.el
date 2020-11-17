;;  -*- lexical-binding: t; -*-
;;; init.el --- Emacs configuration

;;; Commentary:
;;; My Emacs config

;;; Code:

(when (version< emacs-version "26.1")
  (error (concat "This config requires Emacs 26.1+. Current version: " emacs-version)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(when (version< emacs-version "27")
  (package-initialize))

(unless (package-installed-p 'use-package) ;; Make sure use-package is installed
  (package-refresh-contents)
  (package-install 'diminish)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)

(add-to-list 'load-path (concat user-emacs-directory "modules/"))
(add-to-list 'load-path (concat user-emacs-directory "core/"))

(require 'bootstrap)

(core/init)

;; programming

(use-package mod-programming)
(use-package mod-c)
(use-package mod-go)
(use-package mod-javascript)
(use-package mod-emacs-lisp)
(use-package mod-web)
(use-package mod-git)
(use-package mod-rust)

;; writing

(use-package mod-latex)
(use-package mod-markdown)

;; other

(use-package mod-yaml)
(use-package mod-terminal)
(use-package mod-fun)

(provide 'init)
;;; init.el ends here
