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

(use-package programming)
(use-package c)
(use-package go)
(use-package javascript)
(use-package emacs-lisp)
(use-package web)
(use-package git)

;; writing

(use-package latex)
(use-package markdown)

;; other

(use-package yaml)
(use-package terminal)
(use-package fun)

(provide 'init)
;;; init.el ends here
