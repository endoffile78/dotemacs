;;; init.el --- Emacs configuration

;;; Commentary:
;;; My Emacs config

;;; Code:

(when (version< emacs-version "24.4")
  (error (concat "This config requires Emacs 24.4+. Current version: " emacs-version)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Monaco-11"))

(add-to-list 'exec-path "/usr/local/bin")

(unless '(packge-installed-p 'use-package) ;; Make sure use-package is installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'cl))
(require 'diminish)
(require 'bind-key)

(use-package linum)

(define-global-minor-mode my-global-linum-mode linum-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'eshell-mode 'calendar-mode 'term-mode)))
      (linum-mode))))

(my-global-linum-mode)
(global-hl-line-mode 1)
(column-number-mode t)
(recentf-mode)
(blink-cursor-mode 1)
(toggle-save-place-globally)
(show-paren-mode 1)
(electric-pair-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(defvar private-file (concat user-emacs-directory "private.el")
  "Private file that is not tracked.")
(defvar normal-state-color '("#35393B" . "#FFFFFF")
  "Default color for the modeline when in normal mode")
(defvar visual-state-color '("#AB7EFF" . "#000000")
  "Default color for the modeline when in visual mode")
(defvar insert-state-color '("#555555" . "#FFFFFF")
  "Default color for the modeline when in insert mode")
(defvar emacs-state-color '("#FF6159" . "#FFFFFF")
  "Default color for the modeline when in emacs mode")

(setq ring-bell-function 'ignore
      browse-url-browser-function 'browse-url-xdg-open
      inhibit-startup-message t
      initial-scratch-message nil
      auto-save-default nil
      make-backup-files t
      version-control t
      backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
      delete-old-versions t
      vc-follow-symlinks t
      initial-major-mode 'text-mode
      custom-file (concat user-emacs-directory "custom.el")
      ad-redefinition-action 'accept)

(setq-default truncate-lines 1
              backward-delete-function nil
              dired-listing-switches "-alhv"
              dired-recursive-copies 'always
              indent-tabs-mode t
              tab-width 4)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(fset 'yes-or-no-p 'y-or-n-p)

;; Hydra

(use-package hydra
  :ensure)

;; Theme

(use-package darkokai-theme
  :ensure
  :config
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai t))

(use-package doom-theme
  :disabled t
  :ensure
  :init
  (load-theme 'doom-one t))

;; Evil

(defgroup dotemacs-evil nil
  "Configuration options for evil-mode."
  :group 'dotemacs
  :prefix 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-state-minor-modes
  '(git-commit-mode magit-blame-mode)
  "List of minor modes that when active should switch to Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(cl-loop for mode in dotemacs-evil/emacs-state-minor-modes
         do (let ((hook (concat (symbol-name mode) "-hook")))
              (add-hook (intern hook) `(lambda ()
                                         (if ,mode
                                             (evil-emacs-state)
                                           (evil-normal-state))))))

(defun my-evil-modeline-change (default-color)
  "Change the modeline color when the mode changes."
  (let ((color (cond
                ((evil-insert-state-p) insert-state-color)
                ((evil-visual-state-p) visual-state-color)
                ((evil-normal-state-p) normal-state-color)
                ((evil-emacs-state-p) emacs-state-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(use-package evil
  :ensure
  :demand t
  :bind (:map evil-insert-state-map
              ("C-e" . end-of-line)
              ("C-a" . beginning-of-line))
  :config
  (lexical-let ((default-color (cons (face-background 'mode-line)
                                     (face-foreground 'mode-line))))
    (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

  (setq evil-normal-state-cursor '("white" box) ;; Change the cursor color and shape based on the state
        evil-insert-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow))

  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'calculator-mode 'emacs)
  (evil-set-initial-state 'eww-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)

  ;; Vim-like window movement
  (global-unset-key (kbd "C-w"))
  (global-set-key (kbd "C-w <right>") 'evil-window-right)
  (global-set-key (kbd "C-w <left>")  'evil-window-left)
  (global-set-key (kbd "C-w <down>")  'evil-window-down)
  (global-set-key (kbd "C-w <up>")    'evil-window-up)


  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "Q" 'evil-tab-sensitive-quit)

  (evil-mode 1))

(use-package evil-tabs
  :ensure
  :config
  (global-evil-tabs-mode t))

(use-package evil-leader
  :ensure
  :config
  (evil-leader/set-leader ",")
  (global-evil-leader-mode)
  (evil-leader/set-key
    "k"  'kill-this-buffer
    "pf" 'helm-projectile
    "pb" 'helm-projectile-switch-to-buffer
    "pi" 'projectile-invalidate-cache
    "po" 'projectile-find-other-file
    "pk" 'projectile-kill-buffers
    "pg" 'helm-projectile-grep
    "gc" 'ggtags-create-tags
    "gu" 'ggtags-update-tags
    "gf" 'ggtags-find-file
    "gs" 'ggtags-find-other-symbol
    "gt" 'ggtags-find-tag-dwim
    "gg" 'ggtags-grep
    "ms" 'magit-status
    "md" 'magit-diff
    "mb" 'magit-blame
    "ml" 'magit-log-popup
    "mr" 'magit-branch-popup
    "c"  'compile
    "t"  'elscreen-create
    "d"  'gud-gdb
    "fp" 'flyspell-prog-mode
    "hg" 'helm-grep-do-git-grep
    "ha" 'helm-do-grep-ag
    "ir" 'indent-region))

(use-package evil-org
  :diminish evil-org-mode)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode t))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

;; Flycheck

(use-package flycheck
  :ensure
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (defhydra hydra-flycheck ()
    "Flycheck"
    ("l" flycheck-list-errors "list errors" :exit t)
    ("c" flycheck-buffer "check buffer" :exit t)
    ("n" flycheck-next-error "next error")
    ("p" flycheck-previous-error "prev error")
    ("q" nil "quit"))
  (global-set-key (kbd "C-c f") 'hydra-flycheck/body))

;; Git

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :config
  (set-face-foreground 'git-gutter-fr+-modified "yellow")
  (set-face-foreground 'git-gutter-fr+-added    "green")
  (set-face-foreground 'git-gutter-fr+-deleted  "red")
  (global-git-gutter+-mode))

(use-package magit
  :ensure)

(use-package gitignore-mode)

;; Helm

(use-package helm
  :ensure
  :demand t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-c w" . helm-man-woman))
  :config
  (require 'helm-config)
  (setq helm-quick-update t
        helm-bookmark-show-location t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-ff-file-name-history-use-recentf t)
  (helm-mode 1))

(use-package helm-flx
  :config
  (helm-flx-mode +1))

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds))
  :config
  (setq helm-descbinds-window-style 'split-window)
  (helm-descbinds-mode))

;; Company

(use-package company
  :ensure
  :diminish company-mode
  :config
  (setq company-idle-delay 0.1
		company-minimum-prefix-length 2
		company-tooltip-limit 20
		company-global-modes '(not eshell-mode))

  (use-package company-irony)
  (use-package company-irony-c-headers)
  (use-package company-shell)
  (use-package company-cmake)
  (use-package company-jedi)
  (use-package company-tern)
  (use-package company-racer)
  (use-package company-ghc)

  (add-hook 'after-init-hook 'global-company-mode)

  (eval-after-load 'company
	'(add-to-list
	  'company-backends '(company-irony company-irony-c-headers company-yasnippet
                                        company-css company-elisp company-semantic
                                        company-files company-shell company-tern
                                        company-cmake company-jedi company-racer
                                        company-ghc)))

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

;; C

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(c-add-style "my-c-style" '((c-continued-statement-offset 4)
                            (c-tab-always-indent t)
                            (c-toggle-hungry-state t)
                            (c-offsets-alist
                             (inline-open . +)
                             (block-open . +)
                             (brace-list-open . +)
                             (case-label . +)
                             (access-label . /))))

(defun my-c-hook ()
  "Hook for `c-mode'."
  (local-set-key (kbd "C-c d") 'gud-gdb)
  (setq-local indent-tabs-mode t)
  (c-set-style "my-c-style"))

(add-hook 'c-mode-hook #'my-c-hook)
(add-hook 'c++-mode-hook #'my-c-hook)

;; ggtags

(use-package ggtags
  :ensure
  :diminish ggtags-mode
  :commands ggtags-mode
  :init
  (add-hook 'c-mode-common-hook 'ggtags-mode))

;; Irony

(use-package irony
  :ensure
  :commands irony-mode
  :diminish irony-mode
  :preface
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's asynchronous function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook #'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package flycheck-irony
  :ensure
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

;; Rust

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(use-package racer
  :demand t
  :diminish racer-mode
  :bind (:map rust-mode-map
              ("M-." . racer-find-definition))
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  :config
  (setq racer-rust-src-path "~/.rust/src/"))

(use-package flycheck-rust
  :config
  (add-hook 'rust-mode-hook 'flycheck-rust-setup))

;; Python

(defun python-f5 ()
  "Sends the buffer to a python shell."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(use-package python
  :bind (:map python-mode-map
              ("<f5>" . python-f5)
              ("C-c d" . pdb)))

(use-package elpy
  :ensure
  :init
  (elpy-enable))

(use-package virtualenvwrapper
  :config
  (venv-initialize-eshell))

;; Java

(use-package java-file-create)

(use-package ensime
  :config
  (setq ensime-startup-snapshot-notification nil))

;; Markdown

(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

;; Vim

(use-package vimrc-mode
  :mode (".vim\\(rc\\)?$" . vimrc-mode))

;; Programming Utilities

(use-package aggressive-indent
  :disabled
  :diminish aggressive-indent-mode
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode 'c++-mode 'java-mode 'csharp-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  (global-aggressive-indent-mode))

(use-package bool-flip
  :bind ("C-c b" . bool-flip-do-flip))

(use-package clang-format)

(use-package autoinsert
  :config
  (add-hook 'find-file-hook #'auto-insert)
  (setq auto-insert 'other
        auto-insert-query nil
        auto-insert-mode t
        auto-insert-directory (concat user-emacs-directory "auto-insert")
        auto-insert-alist '(("\.html\'" . "template.html")
                            ("^.*html.*$" . "template.html")
                            ('web-mode . "template.html"))))

;; Uniquify

(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*"
      uniquify-after-kill-buffer-p t)

;; Projectile

(use-package projectile
  :ensure
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure
  :config
  (helm-projectile-on))

;; Visual

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package rainbow-delimiters
  :ensure
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure
  :diminish rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

(defhydra hydra-rainbow (:exit t)
  "Rainbow"
  ("m" rainbow-mode "mode")
  ("d" rainbow-delimiters-mode "delimiters")
  ("q" nil "quit"))
(global-set-key (kbd "C-c r") 'hydra-rainbow/body)

;; HTML

(add-hook 'html-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (set (make-local-variable 'sgml-basic-offset) 2)))

(use-package emmet-mode
  :commands emmet-mode
  :diminish emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode))

;; Web-mode

(use-package web-mode
  :ensure
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php?\\'" . web-mode))
  :config
  (defun my-web-mode-hook ()
    (setq-local indent-tabs-mode nil)
    (setq-local electric-pair-pairs '((?\< . ?\>)
                                      (?\' . ?\')))
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 4
          web-mode-code-indent-offset 4
          web-mode-enable-auto-pairing nil
          web-mode-enable-auto-closing t
          web-mode-style-padding 2
          web-mode-script-padding 2
          web-mode-enable-current-element-highlight t
          web-mode-enable-block-face t))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

;; Javascript

(use-package js2-mode
  :ensure
  :mode ("\\.js$" . js2-mode))

;; Yasnippet

(use-package yasnippet
  :ensure
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (defhydra hydra-yasnippet (:exit t)
    "Yasnippet"
    ("g" yas-global-mode "global mode")
    ("m" yas-minor-mode "minor mode")
    ("e" yas-activate-extra-mode "extra mode")
    ("n" yas-new-snippet "new snippet")
    ("r" yas-reload-all "reload")
    ("d" yas-load-directory "load directory")
    ("q" nil "quit"))
  (global-set-key (kbd "C-c y") 'hydra-yasnippet/body))

;; Org

(use-package org
  :config
  (setq org-directory "~/docs/org/")
  (defhydra hydra-org (:hint nil)
    "
^Export^               ^Tables^           ^Movement^
------------------------------------------------------------------
export to _h_tml       create _t_able     _g_oto
export to _l_atex      _d_elete column
export to _p_df        _k_ill row
export to _m_arkdown   insert _c_olumn
                     insert _r_ow

_q_uit
"
    ("h" org-html-export-to-html :exit t)
    ("l" org-latex-export-to-latex :exit t)
    ("p" org-latex-export-to-pdf :exit t)
    ("m" org-md-export-to-markdown :exit t)
    ("t" org-table-create-or-convert-from-region :exit t)
    ("d" org-table-delete-column)
    ("k" org-table-kill-row)
    ("c" org-table-insert-column)
    ("r" org-table-insert-row)
    ("g" org-goto :exit t)
    ("q" nil))
  (global-set-key (kbd "C-c o") 'hydra-org/body))

;; Eldoc

(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

;; undo-tree

(use-package undo-tree
  :diminish undo-tree-mode)

;; Abbrev

(use-package abbrev
  :diminish abbrev-mode)

;; elscreen

(use-package elscreen
  :ensure
  :demand t)

;; Lua

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

;; C#

(use-package csharp-mode
  :mode ("\\.cs$" . csharp-mode))

;; Font

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;; PKGBUILD

(use-package pkgbuild-mode
  :mode ("/PKGBUILD$" . pkgbuild-mode))

;; CMake

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; flyspell

(use-package flyspell
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package flyspell-popup
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-popup-correct)))

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; YAML

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; Haskell

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package flycheck-ghcmod)

(use-package ebal)

;; editorconfig

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Mingus

(use-package mingus-stays-home
  :config
  (defhydra hydra-mingus ()
    "Mingus"
    ("t" mingus-toggle "toggle" :exit t)
    ("i" mingus-insert "insert")
    ("p" mingus-prev "prev")
    ("n" mingus-next "next")
    ("u" mingus-vol-up "up")
    ("d" mingus-vol-down "down")
    ("s" mingus-search "search")
    ("q" nil "quit"))
  (global-set-key (kbd "C-c m") 'hydra-mingus/body))

;; ibuffer

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer-other-window)))

;; Hyperbole

(use-package hyperbole)

;; which key

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;; Emacs Lisp

(defun my-emacs-lisp-mode-hook ()
  "Hook for `emacs-lisp-mode'."
  (interactive)
  (setq-local indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

(defvar sanityinc/theme-mode-hook nil
  "Hook triggered when editing a theme file.")

(defun sanityinc/run-theme-mode-hooks-if-theme ()
  "Run `sanityinc/theme-mode-hook' if this appears to a theme."
  (when (string-match "\\(color-theme-\\|-theme\\.el\\)" (buffer-name))
    (run-hooks 'sanityinc/theme-mode-hook)))

(add-hook 'emacs-lisp-mode-hook #'sanityinc/run-theme-mode-hooks-if-theme)

(add-hook 'sanityinc/theme-mode-hook #'rainbow-mode)
(add-hook 'sanityinc/theme-mode-hook '(lambda () (aggressive-indent-mode -1)))

;; Go

(use-package go-mode)

;; arduino

(use-package arduino-mode
  :mode ("\.ino$" . arduino-mode))

;; Keybindings

(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-region)

(global-set-key (kbd "C-c i") 'insert-char)
(global-set-key (kbd "C-c e") 'eshell)

;; Misc

(use-package fancy-battery-mode
  :init
  (add-hook 'after-init-hook #'fancy-battery-mode))

(use-package immortal-scratch
  :ensure
  :config
  (immortal-scratch-mode))

(use-package dired-k
  :bind (:map dired-mode-map
              ("K" . dired-k)))

(defhydra hydra-scale ()
  "Scale"
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset" :exit t)
  ("q" nil "quit"))
(global-set-key (kbd "C-c s") 'hydra-scale/body)

(load custom-file)

(if (file-exists-p private-file)
    (load private-file))

(provide 'init)
;;; init.el ends here
