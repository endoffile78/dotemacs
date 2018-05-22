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
(electric-pair-mode 1)
(delete-selection-mode)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(menu-bar-mode -1)
(tooltip-mode -1)

;; Variables

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

(if (file-exists-p private-file)
    (load private-file))

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
              indent-tabs-mode t
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
  :load-path "themes/darkokai"
  :config
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai t))

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
  (evil-set-initial-state 'snake-mode 'emacs)
  (evil-set-initial-state 'mu4e-headers-mode 'emacs)
  (evil-set-initial-state 'mu4e-compose-mode 'emacs)
  (evil-set-initial-state 'cider-repl-mode 'emacs)
  (evil-set-initial-state 'stacktrace-mode 'emacs)
  (evil-set-initial-state 'erc-mode 'emacs)
  (evil-set-initial-state 'gnus-summary-mode 'emacs)
  (evil-set-initial-state 'gnus-article-mode 'emacs)

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
    "pt" 'projectile-run-term
    "gc" 'ggtags-create-tags
    "gu" 'ggtags-update-tags
    "gf" 'ggtags-find-file
    "gs" 'ggtags-find-other-symbol
    "gt" 'ggtags-find-tag-dwim
    "ms" 'magit-status
    "md" 'magit-diff
    "mb" 'magit-blame
    "ml" 'magit-log-popup
    "mr" 'magit-branch-popup
    "mm" 'magit-merge-popup
    "t"  'elscreen-create
    "fp" 'flyspell-prog-mode
    "hg" 'helm-grep-do-git-grep
    "ha" 'helm-do-grep-ag
    "hr" 'helm-recentf
    "ir" 'indent-region
    "x"  'helm-M-x))

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

(use-package evil-mc
  :diminish evil-mc-mode
  :config
  (global-evil-mc-mode 1))

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
        company-tooltip-limit 20)

  (use-package company-irony
    :config
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

  (use-package company-irony-c-headers)
  (use-package company-shell)
  (use-package company-cmake)
  (use-package company-jedi)
  (use-package company-racer)
  (use-package company-ghc)

  (add-hook 'prog-mode-hook 'company-mode)

  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-irony company-irony-c-headers company-yasnippet
                                        company-css company-elisp company-semantic
                                        company-files company-shell company-cmake
                                        company-jedi company-racer company-ghc))))

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

(use-package disaster
  :ensure)

(defun my-c-hook ()
  "Hook for `c-mode'."
  (local-set-key (kbd "C-c d") 'gud-gdb)
  (local-set-key (kbd "C-c c") 'compile)
  (local-set-key (kbd "C-c o") 'disaster)
  (setq-local indent-tabs-mode nil)
  (c-set-style "my-c-style"))

(add-hook 'c-mode-hook #'my-c-hook)
(add-hook 'c++-mode-hook #'my-c-hook)

(evil-leader/set-key-for-mode 'c-mode "c" 'compile)
(evil-leader/set-key-for-mode 'c-mode "d" 'gud-gdb)
(evil-leader/set-key-for-mode 'c++-mode "c" 'compile)
(evil-leader/set-key-for-mode 'c++-mode "d" 'gud-gdb)

;; Makefile

(defun my-makefile-hook ()
  "Hook for `makefile-mode'."
  (local-set-key (kbd "C-c c") 'compile)
  (setq-local indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'my-makefile-hook)

(evil-leader/set-key-for-mode 'makefile-mode "c" 'compile)

;; ggtags

(use-package ggtags
  :ensure
  :diminish ggtags-mode
  :commands ggtags-mode
  :init
  (add-hook 'c-mode-common-hook 'ggtags-mode)
  (add-hook 'php-mode-hook 'ggtags-mode))

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
  :config
  (setq racer-rust-src-path "~/.rust/src/")
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode))

(use-package company-racer
  :config
  (add-to-list 'company-backends 'company-racer))

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
  :config
  (elpy-enable))

(use-package virtualenvwrapper
  :config
  (evil-leader/set-key-for-mode 'python-mode "va" 'venv-workon)
  (evil-leader/set-key-for-mode 'python-mode "vd" 'venv-deactivate)
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package pip-requirements)

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
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'trailing-whitespace)

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

;; HTML

(add-hook 'html-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (set (make-local-variable 'sgml-basic-offset) 2)))

(use-package emmet-mode
  :commands emmet-mode
  :diminish emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

;; Web-mode

(use-package web-mode
  :ensure
  :mode ("\\.html?\\'" . web-mode)
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
  :bind (:map org-mode-map
              ("C-c o h" . hydra-org/body))
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
          ("DONE" ("WAITING") ("CANCELLED"))))

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

  (global-set-key (kbd "C-c o a") 'org-agenda)
  (global-set-key (kbd "C-c o c") 'org-capture))

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
  :bind (("C-c e t" . elscreen-create)
         ("C-c e n" . elscreen-next)
         ("C-c e p" . elscreen-previous))
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
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-popup-correct)))

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'tex-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; LaTeX

(use-package tex
  :ensure auctex)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(evil-leader/set-key-for-mode 'latex-mode "c" 'TeX-command-master)

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
    ("s" mingus-search "search" :exit t)
    ("q" nil "quit"))
  (global-set-key (kbd "C-c m") 'hydra-mingus/body))

;; ibuffer

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer-other-window))
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

(use-package cider)

(use-package elein
  :bind (:map clojure-mode-map
              ("C-c c" . elein-compile))
  :config
  (evil-leader/set-key-for-mode 'clojure-mode "c" 'elein-compile))

(use-package flycheck-clojure
  :config
  (flycheck-clojure-setup))

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

;; Keybindings

(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-region)

(global-set-key (kbd "C-c u") 'insert-char)
(global-set-key (kbd "C-c s") 'term)

;; dired

(setq dired-listing-switches "-alhv"
      dired-recursive-copies 'always)

(use-package dired-icon
  :config
  (add-hook 'dired-mode-hook 'dired-icon-mode))

(use-package dired-details+)

;; mu4e

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :config
  (setq mu4e-maildir "~/mail"
        mu4e-get-mail-command "offlineimap"
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        mu4e-decryption-policy t
        mu4e-headers-skip-duplicates t
        message-kill-buffer-on-exit t
        mu4e-use-fancy-chars t
        mu4e-sent-messages-behavior 'delete
        mu4e-mu-binary "/usr/bin/mu"))

(use-package mu4e-alert
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook 'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook 'mu4e-alert-enable-mode-line-display))

;; Misc

(use-package fancy-battery
  :init
  (add-hook 'after-init-hook 'fancy-battery-mode))

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
(global-set-key (kbd "C-c z") 'hydra-scale/body)

(load custom-file)

(provide 'init)
;;; init.el ends here
