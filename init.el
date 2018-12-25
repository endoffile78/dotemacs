;;; init.el --- Emacs configuration

;;; Commentary:
;;; My Emacs config

;;; Code:

(when (version< emacs-version "26.1")
  (error (concat "This config requires Emacs 26.1+. Current version: " emacs-version)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Monaco-11"))

(if (eq (system-name) 'gnu/linux)
    (add-to-list 'exec-path "/usr/local/bin"))

(unless '(packge-installed-p 'use-package) ;; Make sure use-package is installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'cl))
(require 'diminish)
(require 'bind-key)

(use-package linum)

;; Global Modes

(define-global-minor-mode my-global-linum-mode linum-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'eshell-mode 'calendar-mode 'term-mode
                           'doc-view-mode 'erc-mode)))
      (linum-mode))))

(my-global-linum-mode)
(global-hl-line-mode 1)
(column-number-mode t)
(recentf-mode)
(blink-cursor-mode 1)
(toggle-save-place-globally)
(show-paren-mode 1)
(delete-selection-mode)
(winner-mode)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(menu-bar-mode -1)
(tooltip-mode -1)

;; Variables

(defvar private-file (concat user-emacs-directory "private.el")
  "Private file that is not tracked.")
(defvar local-file (concat user-emacs-directory "local.el")
  "Local file specific to each computer.")
(defvar normal-state-color '("#35393B" . "#FFFFFF")
  "Default color for the modeline when in normal mode")
(defvar visual-state-color '("#AB7EFF" . "#000000")
  "Default color for the modeline when in visual mode")
(defvar insert-state-color '("#555555" . "#FFFFFF")
  "Default color for the modeline when in insert mode")
(defvar emacs-state-color '("#FF6159" . "#FFFFFF")
  "Default color for the modeline when in emacs mode")

(if (file-exists-p private-file)
    (load private-file))

(if (file-exists-p local-file)
    (load local-file))

;; Settings

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
      ad-redefinition-action 'accept
      custom-safe-themes t)

(setq-default truncate-lines 1
              backward-delete-function nil
              indent-tabs-mode nil
              tab-width 4
              require-final-newline t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(fset 'yes-or-no-p 'y-or-n-p)

;; Hydra

(use-package hydra
  :ensure)

;; Theme

(use-package darkokai-theme
  :config
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai t))

(use-package doom-modeline
  :config
  (add-hook 'after-init-hook 'doom-modeline-init))

;; ace-window

(use-package ace-window)

;; General

(use-package general
  :ensure
  :config
  (general-create-definer leader-define
    :prefix ",")
  (leader-define
    :states 'normal
    ;; buffer managment
    "b" '(:ignore t :which-key "Buffer Management")
    "bb" 'switch-to-buffer
    "bk" 'kill-this-buffer

    ;; projectile
    "p" '(:ignore t :which-key "Projectile")
    "pf" 'helm-projectile
    "pb" 'helm-projectile-switch-to-buffer
    "po" 'projectile-find-other-file
    "pk" 'projectile-kill-buffers
    "pt" 'projectile-run-term

    ;; magit
    "m" '(:ignore t :which-key "Magit")
    "ms" 'magit-status
    "md" 'magit-diff
    "mb" 'magit-blame
    "ml" 'magit-log-popup
    "mr" 'magit-branch-popup
    "mm" 'magit-merge-popup

    ;; org
    "o" '(:ignore t :which-key "Org")
    "oa" 'org-agenda
    "oc" 'org-capture

    ;; files
    "f" '(:ignore t :which-key "Files")
    "ff" 'find-file
    "fi" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "fp" '(lambda () (interactive) (find-file "~/.emacs.d/private.el"))

    ;; window management
    "w" '(:ignore t :which-key "Window Management")
    "wu" 'winner-undo
    "wr" 'winner-redo
    "wh" 'windmove-left
    "wj" 'windmove-down
    "wk" 'windmove-up
    "wl" 'windmove-right
    "wd" 'ace-delete-window
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit
    "ww" 'ace-select-window

    "hg" 'helm-grep-do-git-grep
    "hr" 'helm-recentf
    "x"  'helm-M-x)
  (general-define-key
   :states '(normal insert emacs)
   "C-a" 'beginning-of-line
   "C-e" 'end-of-line))

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
  :init
  (setq evil-want-keybinding nil) ;; disable for evil-collection
  :config
  (lexical-let ((default-color (cons (face-background 'mode-line)
                                     (face-foreground 'mode-line))))
    (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

  (setq evil-normal-state-cursor '("white" box) ;; Change the cursor color and shape based on the state
        evil-insert-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow))

  (evil-set-initial-state 'snake-mode 'emacs)
  (evil-set-initial-state 'cider-repl-mode 'emacs)
  (evil-set-initial-state 'stacktrace-mode 'emacs)
  (evil-set-initial-state 'erc-mode 'emacs)

  (evil-set-initial-state 'term-mode 'insert)
  (evil-set-initial-state 'shell-mode 'insert)
  (evil-set-initial-state 'eshell-mode 'insert)

  ;; Vim-like window movement
  (global-unset-key (kbd "C-w"))
  (general-define-key
   "C-w <right>" 'evil-window-right
   "C-w <left>"  'evil-window-left
   "C-w <down>"  'evil-window-bottom
   "C-w <up>"    'evil-window-up)

  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "Q" 'evil-tab-sensitive-quit)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-tabs
  :ensure
  :config
  (global-evil-tabs-mode t))

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

(use-package evil-avy
  :ensure
  :config
  (evil-avy-mode))

(use-package evil-smartparens
  :after smartparens
  :ensure
  :diminish evil-smartparens-mode
  :config
  (add-hook 'smartparens-mode-hook 'evil-smartparens-mode))

;; Flycheck

(use-package flycheck
  :ensure
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (defhydra hydra-flycheck ()
    "Flycheck"
    ("l" flycheck-list-errors "list errors" :exit t)
    ("c" flycheck-buffer "check buffer" :exit t)
    ("n" flycheck-next-error "next error")
    ("p" flycheck-previous-error "prev error")
    ("q" nil "quit"))
  (general-define-key "C-c f" 'hydra-flycheck/body))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 30)
  (flycheck-pos-tip-mode))

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
  :general ("M-x"     'helm-M-x)
  ("C-c w"   'helm-man-woman)
  ("C-x C-f" 'helm-find-files)
  :config
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
  :general ("C-h b" 'helm-descbinds)
  :config
  (setq helm-descbinds-window-style 'split-window)
  (helm-descbinds-mode))

(use-package helm-projectile
  :after projectile
  :ensure
  :config
  (helm-projectile-on))

;; Company

(use-package company
  :ensure
  :diminish company-mode
  :general (:keymaps 'company-active-map
                     "<tab>" 'company-select-next
                     "TAB" 'company-select-next
                     "<backtab>" 'company-select-previous)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-limit 20)

  (use-package company-irony
    :config
    (setq company-irony-ignore-case t)
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

  (use-package company-irony-c-headers)
  (use-package company-shell)
  (use-package company-cmake)
  (use-package company-ghc)

  (add-hook 'prog-mode-hook 'company-mode)

  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-irony company-irony-c-headers company-yasnippet
                                        company-css company-elisp company-semantic
                                        company-files company-shell company-cmake
                                        company-ghc))))

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

(setq c-default-style
      (quote
       ((c-mode . "my-c-style")
        (c++-mode . "my-c-style")
        (java-mode . "java")
        (awk-mode . "awk"))))

(leader-define
  :states 'normal
  :keymaps '(c-mode-map c++-mode-map)
  "c" 'compile
  "d" 'gud-gdb)

(general-define-key
 :keymaps '(c-mode-map c++-mode-map)
 "C-c c" 'compile
 "C-c d" 'gud-gdb)

;; Makefile

(defun my-makefile-hook ()
  "Hook for `makefile-mode'."
  (setq-local indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'my-makefile-hook)

;; ggtags

(use-package ggtags
  :ensure
  :diminish ggtags-mode
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  (leader-define
    :states 'normal
    :keymaps '(c-mode-map c++-mode-map)
    "g" '(:ignore t :which-key "ggtags")
    "gc" 'ggtags-create-tags
    "gu" 'ggtags-update-tags
    "gf" 'ggtags-find-file
    "gs" 'ggtags-find-other-symbol
    "gt" 'ggtags-find-tag-dwim))

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
  (defun my-irony-enable ()
    (when (memq major-mode irony-supported-major-modes)
      (irony-mode 1)))
  :init
  (add-hook 'c++-mode-hook 'my-irony-enable)
  (add-hook 'c-mode-hook 'my-irony-enable)
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

;; eglot

(use-package eglot
  :config
  (leader-define
    :states 'normal
    :keymaps 'eglot-mode-map
    "er" 'eglot-rename)
  (add-hook 'python-mode-hook 'eglot-ensure))

;; Python

(defun python-f5 ()
  "Sends the buffer to a python shell."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(use-package python
  :config
  :general (:keymaps 'python-mode-map
                     "<f5>" 'python-f5))

(use-package virtualenvwrapper
  :config
  (leader-define
    :states 'normal
    :keymaps 'python-mode-map
    "va" 'venv-workon
    "vd" 'venv-deactivate)
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package pip-requirements)

;; Java

(use-package java-file-create)

;; Markdown

(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

;; Vim

(use-package vimrc-mode
  :mode (".vim\\(rc\\)?$" . vimrc-mode))

;; Programming Utilities

(use-package autoinsert
  :config
  (add-hook 'find-file-hook 'auto-insert)
  (setq auto-insert 'other
        auto-insert-query nil
        auto-insert-mode t
        auto-insert-directory (concat user-emacs-directory "auto-insert")
        auto-insert-alist '(("\.html\'" . "template.html")
                            ("^.*html.*$" . "template.html")
                            ("\.php\'" . "template.php")
                            ("^.*php.*$" . "template.php"))))

(use-package comment-tags
  :init
  (setq comment-tags-keymap-prefix (kbd "C-c t"))
  :config
  (setq comment-tags-keyword-faces
        `(("TODO"  . ,(list :weight 'bold :foreground "#28ABE3"))
          ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
          ("BUG"   . ,(list :weight 'bold :foreground "#DB3340"))
          ("HACK"  . ,(list :weight 'bold :foreground "#E8B71A"))
          ("INFO"  . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("DONE"  . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil)
  (add-hook 'prog-mode-hook 'comment-tags-mode))

(defun trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'trailing-whitespace)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; smartparens

(use-package smartparens-config
  :ensure smartparens
  :config
  (sp-with-modes
      '(c++-mode objc-mode c-mode)
    (sp-local-pair "/*" "*/" :post-handlers
                   '(:add
                     ("* [i]|\n[i]" newline evil-ret)
                     (" " c-context-line-break c-indent-new-comment-line)))
    (sp-local-pair "<" ">"))
  (sp-with-modes
      '(c++-mode objc-mode c-mode css-mode js2-mode web-mode java-mode)
    (sp-local-pair "{" nil :post-handlers
                   '(:add
                     ("||\n[i]" "RET")
                     ("| " "SPC"))))
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-escape-quotes-after-insert nil)
  (sp-use-paredit-bindings)
  (smartparens-global-mode))

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

;; HTML

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
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 4
          web-mode-code-indent-offset 4
          web-mode-enable-auto-pairing nil
          web-mode-enable-auto-closing t
          web-mode-enable-auto-quoting t
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
    ("i" yas-insert-snippet "insert snippet")
    ("n" yas-new-snippet "new snippet")
    ("r" yas-reload-all "reload")
    ("q" nil "quit"))
  (general-define-key "C-c y" 'hydra-yasnippet/body))

;; Org

(use-package org
  :config
  (setq org-directory "~/docs/org/")
  (setq org-default-notes-file "~/docs/org/notes.org")
  (setq org-agenda-files `(,org-directory))
  (setq org-log-done t)
  (setq org-startup-indented t)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (expand-file-name org-default-notes-file) "TODOS")
           "* TODO %?\n%U\n%a\n")
          ("n" "Note" entry (file+headline (expand-file-name org-default-notes-file) "NOTES")
           "* %? :NOTE:\n%U\n%a\n")))
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("TODO" ("WAITING") ("CANCELLED"))
          ("DONE" ("WAITING") ("CANCELLED")))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Eldoc

(use-package eldoc
  :diminish eldoc-mode
  :config
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
  :ensure
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; Bazel

(use-package bazel-mode)

;; flyspell

(use-package flyspell
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'latex-mode-hook 'flyspell-mode)
  (add-hook 'tex-mode-hook 'flyspell-mode))

(use-package flyspell-popup
  :general (:keymaps 'flyspell-mode-map
                     "C-;" 'flyspell-popup-correct))

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'tex-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; LaTeX

(use-package tex
  :ensure auctex)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

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

;; ibuffer

(use-package ibuffer
  :general ("C-x C-b" 'ibuffer-other-window)
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
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; which key

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-allow-evil-operators t)
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

;; tramp

(setq tramp-default-method "ssh")

;; Go

(use-package go-mode)

;; arduino

(use-package arduino-mode
  :mode ("\.ino$" . arduino-mode))

;; nginx

(use-package nginx-mode
  :config
  (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

;; clojure

(use-package clojure-mode)

(use-package clojure-mode-extra-font-locking)

;; IRC

(use-package erc
  :config
  (use-package erc-log
    :config
    (setq erc-log-channels-directory "~/.erc/logs/")
    (add-to-list 'erc-modules 'log))

  (use-package erc-autoaway
    :config
    (setq erc-auto-discard-away t
          erc-autoaway-idle-seconds 600
          erc-autoaway-use-emacs-idle t))

  (use-package erc-spelling
    :config
    (erc-spelling-mode 1))

  (use-package erc-hl-nicks
    :ensure)

  (use-package erc-image
    :ensure
    :config
    (add-to-list 'erc-modules 'image))

  (use-package erc-fill
    :config
    (setq erc-fill-static-center 20
          erc-fill-column 170
          erc-fill-function 'erc-fill-static)
    (erc-fill-mode +1))

  (erc-track-mode t)
  (erc-truncate-mode +1)

  (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))

  (setq erc-format-query-as-channel-p t
        erc-track-priority-faces-only 'all
        erc-track-faces-priority-list '(erc-error-face
                                        erc-current-nick-face
                                        erc-keyword-face
                                        erc-nick-msg-face
                                        erc-direct-msg-face
                                        erc-dangerous-host-face
                                        erc-notice-face
                                        erc-prompt-face)
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-kill-buffer-on-part t
        erc-kill-server-buffer-on-quit t
        erc-kill-queries-on-quit t
        erc-nick "e0f"
        erc-timestamp-format "[%H:%M] "
        erc-insert-timestamp-function 'erc-insert-timestamp-left))

(defun do-notify (nickname message)
  (let* ((channel (buffer-name))
         (nick (erc-hl-nicks-trim-irc-nick nickname))
         (title (if (string-match-p (concat "^" nickname) channel)
                    nick
                  (concat nick " (" channel ")")))
         (msg (s-trim (s-collapse-whitespace message))))
    (notifications-notify
     :title title
     :body (format "%s said %s" nick msg))))

(use-package ercn
  :ensure
  :config
  (setq ercn-notify-rules
        '((current-nick . all)
          (keyword . all)
          (query-buffer . all)))
  (add-hook 'ercn-notify-hook 'do-notify))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Keybindings

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 "C-j" 'eval-region)

(leader-define
  :states '(normal visual)
  :keymaps 'emacs-lisp-mode-map
  "e" '(:ignore t :which-key "Emacs Lisp")
  "eb" 'eval-buffer
  "er" 'eval-region
  "ee" 'eval-expression
  "ed" 'eval-defun)

(general-define-key
 "C-c s" 'term
 "C-x C-r" 'sudo-edit)

;; dired

(setq dired-listing-switches "-alhv"
      dired-recursive-copies 'always)

;; mu4e

(if (eq (system-name) 'gnu/linux)
    (use-package mu4e
      :load-path "/usr/share/emacs/site-lisp/mu4e"
      :init
      (setq mu4e-maildir "~/mail"
            message-send-mail-function 'message-send-mail-with-sendmail
            mu4e-decryption-policy t
            mu4e-headers-skip-duplicates t
            message-kill-buffer-on-exit t
            mu4e-use-fancy-chars t
            mu4e-sent-messages-behavior 'delete
            mu4e-mu-binary "/usr/bin/mu"
            mu4e-view-show-addresses t
            message-kill-buffer-on-exit t
            mu4e-get-mail-command "mbsync -a"))
  (use-package mu4e-alert
    :config
    (mu4e-alert-set-default-style 'libnotify)
    (add-hook 'after-init-hook 'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook 'mu4e-alert-enable-mode-line-display)))

;; Misc

(if local/laptop ;;only show the battery in the modeline if its a laptop
    (use-package fancy-battery
      :init
      (add-hook 'after-init-hook 'fancy-battery-mode)))

(use-package immortal-scratch
  :ensure
  :config
  (immortal-scratch-mode))

(use-package zone-nyan)

(defhydra hydra-scale ()
  "Scale"
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset" :exit t)
  ("q" nil "quit"))
(general-define-key "C-c z" 'hydra-scale/body)

(load custom-file)

(provide 'init)
;;; init.el ends here
