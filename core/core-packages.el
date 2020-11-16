;;; core-packages.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/endoffile>
;; Maintainer:  <endoffile@localhost>
;; Created: November 05, 2020
;; Modified: November 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/endoffile/core-packages
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Packages I always want enabled
;;
;;; Code:

(use-package swiper
  :ensure
  :bind ("C-s" . swiper))

(use-package ivy
  :ensure
  :bind ("C-c C-r" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(use-package ivy-rich
  :ensure
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel
  :ensure
  :bind ("M-x" . counsel-M-x)
  :general
  (leader-def
    :states 'normal
    "SPC" 'counsel-M-x

    "s" '(:ignore t :which-key "Search")
    "ss" 'swiper
    "sf" 'counsel-rg
    "si" 'counsel-imenu))

(use-package immortal-scratch
  :ensure
  :config
  (immortal-scratch-mode))

(use-package projectile
  :ensure
  :general
  (leader-def
    "p" '(:ignore t :which-key "projects")
    "pf" 'projectile-find-file
    "pb" 'projectile-switch-to-buffer
    "pk" 'projectile-kill-buffers
    "pt" 'projectile-run-term
    "pi" 'projectile-invalidate-cache)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-cache-file (concat core/cache-dir "projectile.cache"))
  (projectile-mode))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer-other-window)
  :config
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("org" (mode . org-mode))
                 ("IRC" (mode . erc-mode))
                 ("shell" (or (mode . term-mode)
                              (mode . shell-mode)
                              (mode . eshell-mode)))
                 ("mu4e" (or (mode . mu4e-compose-mode)
                             (name . "\*mu4e\*")))
                 ("Emacs" (or (name . "^\\*scratch\\*$")
                              (name . "^\\*Messages\\*$")
                              (name . "^\\*Packages\\*$")
                              (name . "^\\*Help\\*$"))))))))

(use-package ibuffer-projectile
  :ensure
  :after projectile
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package ws-butler
  :ensure
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package editorconfig
  :ensure
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package rainbow-delimiters
  :ensure
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rg
  :ensure
  :general
  (leader-def
    "/" 'rg)
  :config
  (rg-enable-default-bindings))

(use-package yasnippet
  :ensure
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (concat core/etc-dir "snippets")))
  (yas-global-mode 1))

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :config
  (add-hook 'prog-mode-hook 'eldoc-mode))

(use-package autoinsert
  :config
  (add-hook 'find-file-hook 'auto-insert)
  (setq auto-insert 'other
        auto-insert-query nil
        auto-insert-mode t
        auto-insert-alist '(("\.html\'" . "template.html")
                            ("^.*html.*$" . "template.html")
                            ("\.php\'" . "template.php")
                            ("^.*php.*$" . "template.php"))))

(use-package flycheck
  :ensure
  :commands flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flycheck-pos-tip
  :ensure
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 30)
  (flycheck-pos-tip-mode))

(use-package flyspell
  :config
  (evil-define-key '(normal visual) 'flyspell-mode-map (kbd "z=") 'flyspell-correct-at-point)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package comment-tags
  :ensure
  :init
  (setq comment-tags-keymap-prefix (kbd "C-c t"))
  :config
  (setq comment-tags-keyword-faces
        `(("TODO"  . ,(list :weight 'bold :foreground "#28ABE3"))
          ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
          ("BUG"   . ,(list :weight 'bold :foreground "#DB3340"))
          ("HACK"  . ,(list :weight 'bold :foreground "#DB3340"))
          ("INFO"  . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("DONE"  . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil)
  (add-hook 'prog-mode-hook 'comment-tags-mode))

(use-package company
  :ensure
  :diminish company-mode
  :bind (:map company-active-map
              ("<tab>" . company-complete-common-or-cycle)
              ("TAB" . company-complete-common-or-cycle)
              ("<backtab>" . company-select-previous))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-limit 20)

  (setq company-backends
        '((company-files
           company-keywords
           company-capf
           company-yasnippet)))

  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-quickhelp
  :ensure
  :config
  (company-quickhelp-mode 1))

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode)

(use-package org
  :config
  (setq org-directory "~/docs/org/"
        org-default-notes-file "~/docs/org/notes.org"
        org-agenda-files `(,org-directory)
        org-log-done t
        org-startup-indented t))

(use-package ox-reveal
  :ensure)

(use-package perspective
  :ensure
  :config
  (persp-mode))

(use-package persp-projectile
  :ensure)

(provide 'core-packages)
;;; core-packages.el ends here
