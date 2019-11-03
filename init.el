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

(if (eq (system-name) 'gnu/linux)
    (progn
      (add-to-list 'exec-path "~/bin")
      (add-to-list 'exec-path "/usr/local/bin")))

(unless (package-installed-p 'use-package) ;; Make sure use-package is installed
  (package-refresh-contents)
  (package-install 'diminish)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'cl))
(require 'diminish)

;; general config

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
      custom-safe-themes t
      find-file-visit-truename t
      dired-listing-switches "-alh"
      dired-recursive-copies 'always
      tramp-default-method "ssh"
      default-directory (getenv "HOME"))

(setq-default truncate-lines 1
              backward-delete-function nil
              indent-tabs-mode nil
              tab-width 4
              require-final-newline t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(fset 'yes-or-no-p 'y-or-n-p)

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

(use-package immortal-scratch
  :ensure
  :config
  (immortal-scratch-mode))

;; appearance

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Monaco-11"))

(use-package darkokai-theme
  :ensure
  :config
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai t))

(defvar my-dark-theme 'darkokai)
(defvar my-light-theme 'hydandata-light)
(defvar my-current-theme my-dark-theme)

(defun my/switch-theme (theme)
  (disable-theme my-current-theme)
  (setq my-current-theme theme)
  (load-theme theme t))

(defun my/cycle-theme()
  (interactive)
  (cond
   ((eq my-current-theme my-dark-theme)
    (my/switch-theme my-light-theme))
   ((eq my-current-theme my-light-theme)
    (my/switch-theme my-dark-theme))))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(use-package doom-modeline
  :ensure
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (add-hook 'after-init-hook 'doom-modeline-init))

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package rainbow-delimiters
  :ensure
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; keybindings

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

(use-package general
  :ensure
  :config
  (general-override-mode 1)
  ;; leader keybindings
  (general-create-definer leader-define
    :states '(normal operator visual insert emacs)
    :prefix ","
    :non-normal-prefix "C-,")
  (leader-define
    :states 'normal
    "" nil

    "c" (general-simulate-key "C-c")
    "x" (general-simulate-key "C-x")
    "h" (general-simulate-key "C-h")
    "u" (general-simulate-key "C-u")

    ;; buffer managment
    "b" '(:ignore t :which-key "Buffer Management")
    "bb" 'switch-to-buffer
    "bk" 'kill-this-buffer

    "d" 'dired

    ;; org
    "o" '(:ignore t :which-key "Org")
    "oa" 'org-agenda
    "oc" 'org-capture

    ;; files
    "f" '(:ignore t :which-key "Files")
    "ff" 'find-file
    "fi" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "fp" '(lambda () (interactive) (find-file "~/.emacs.d/private.el"))
    "fl" '(lambda () (interactive) (find-file "~/.emacs.d/local.el"))

    "t" '(:ignore t :which-key "Themes")
    "tc" 'my/cycle-theme

    ;; window management
    "w" '(:ignore t :which-key "Window Management")
    "wu" 'winner-undo
    "wr" 'winner-redo
    "wh" 'windmove-left
    "wj" 'windmove-down
    "wk" 'windmove-up
    "wl" 'windmove-right)

  (general-define-key
   :states '(normal insert emacs)
   "C-a" 'beginning-of-line
   "C-e" 'end-of-line)

  (general-define-key "C-x C-r" 'sudo-edit))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;; evil

(defvar normal-state-color '("#35393B" . "#FFFFFF")
  "Default color for the modeline when in normal mode")
(defvar visual-state-color '("#AB7EFF" . "#000000")
  "Default color for the modeline when in visual mode")
(defvar insert-state-color '("#555555" . "#FFFFFF")
  "Default color for the modeline when in insert mode")
(defvar emacs-state-color '("#FF6159" . "#FFFFFF")
  "Default color for the modeline when in emacs mode")

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

(defun my-evil-theme-change ()
  (cond
   ((eq my-current-theme my-dark-theme)
    (setq evil-normal-state-cursor '("white" box)
          evil-insert-state-cursor '("red" bar)
          evil-operator-state-cursor '("red" hollow))
    (setq normal-state-color '("#35393B" . "#FFFFFF")
          visual-state-color '("#AB7EFF" . "#000000")
          insert-state-color '("#555555" . "#FFFFFF")
          emacs-state-color '("#FF6159" .  "#FFFFFF")))
   ((eq my-current-theme my-light-theme)
    (setq evil-normal-state-cursor '("black" box)
          evil-insert-state-cursor '("red" bar)
          evil-operator-state-cursor '("red" hollow))
    (setq normal-state-color '("#BCD5FA" . "#000000")
          visual-state-color '("#AB7EFF" . "#000000")
          insert-state-color '("#D0E1FB" . "#000000")
          emacs-state-color '("#FF6159" .  "#FFFFFF")))))

(add-hook 'after-load-theme-hook 'my-evil-theme-change)

(use-package evil
  :general
  (leader-define
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit)
  :ensure
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; disable for evil-collection
  :config
  (lexical-let ((default-color (cons (face-background 'mode-line)
                                     (face-foreground 'mode-line))))
    (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

  (setq evil-normal-state-cursor '("white" box)
        evil-insert-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow))

  (evil-set-initial-state 'snake-mode 'emacs)
  (evil-set-initial-state 'stacktrace-mode 'emacs)

  (evil-set-initial-state 'term-mode 'insert)
  (evil-set-initial-state 'shell-mode 'insert)
  (evil-set-initial-state 'eshell-mode 'insert)

  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "Q" 'evil-tab-sensitive-quit)

  (evil-mode 1))

(use-package evil-collection
  :ensure
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(if (version< emacs-version "27")
    (use-package evil-tabs
      :ensure
      :config
      (global-evil-tabs-mode t))
  (tab-bar-mode))

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
  (add-hook 'prog-mode-hook 'evil-commentary-mode))

(use-package evil-smartparens
  :after smartparens
  :ensure
  :diminish evil-smartparens-mode
  :config
  (add-hook 'smartparens-mode-hook 'evil-smartparens-mode))

(use-package evil-easymotion
  :general
  (:states '(normal visual operator)
           "f" 'evilem-motion-find-char
           "F" 'evilem-motion-find-char-backward
           "t" 'evilem-motion-find-char-to
           "T" 'evilem-motion-find-char-to-backward
           "(" 'evilem-motion-backward-sentence-begin
           ")" 'evilem-motion-forward-sentence-begin)
  (leader-define
    "j" 'evilem-motion-next-line
    "k" 'evilem-motion-previous-line)
  :ensure)

(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds))

(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :config
    (evil-terminal-cursor-changer-activate)))

;; navigation

(setq recentf-max-saved-items 50)
(setq recentf-exclude '("/elpa/"
                        "/games/"))

(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*"
      uniquify-after-kill-buffer-p t)

(use-package ibuffer
  :general
  (leader-define "bl" 'ibuffer-other-window)
  ("C-x C-b" 'ibuffer-other-window)
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
  :after projectile
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package ace-window
  :general
  (leader-define
    "wd" 'ace-delete-window
    "ww" 'ace-select-window))

;; ivy

(use-package swiper
  :ensure
  :general ("C-s" 'swiper))

(use-package ivy
  :ensure
  :general ("C-c C-r" 'ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(use-package counsel
  :ensure
  :general ("M-x" 'counsel-M-x))

;; semantic-mode

(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c++-mode-hook 'semantic-mode)
(add-hook 'java-mode-hook 'semantic-mode)

(use-package srefactor
  :general
  (leader-define
    :keymaps 'semantic-mode-map
    "sr" 'srefactor-refactor-at-point))

;; C/C++

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

(defun my-makefile-hook ()
  "Hook for `makefile-mode'."
  (setq-local indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'my-makefile-hook)

(use-package cmake-mode
  :ensure
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(leader-define
  :keymaps '(c-mode-map c++-mode-map)
  "m"  '(:ignore t :which-key "C/C++")
  "mc" 'compile
  "md" 'gud-gdb)

(general-define-key
 :keymaps '(c-mode-map c++-mode-map)
 "C-c c" 'compile
 "C-c d" 'gud-gdb)

;; Go

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :ensure
  :config
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook 'subword-mode)
  (add-hook 'before-save-hook 'gofmt-before-save))

;; python

(defun python-f5 ()
  "Sends the buffer to a python shell."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(use-package python
  :config
  :general
  (leader-define
    :keymaps 'python-mode-map
    "m" '(:ignore t :which-key "Python")
    "ma" 'venv-workon
    "md" 'venv-deactivate)
  (:keymaps 'python-mode-map
            "<f5>" 'python-f5))

(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package pip-requirements)

;; shell

(use-package powershell)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq-default shell-file-name "/bin/zsh")

(general-define-key "C-c s" 'term)

;; web

(use-package rainbow-mode
  :ensure
  :diminish rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

(use-package emmet-mode
  :commands emmet-mode
  :diminish emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package web-mode
  :ensure
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php?\\'" . web-mode)
         ("\\.aspx?\\'" . web-mode))
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

(use-package js2-mode
  :ensure
  :mode ("\\.js$" . js2-mode))

(use-package typescript-mode)

(use-package tide
  :config
  (defun my-tide-hook ()
    (tide-setup)
    (tide-hl-identifier-mode))
  (add-hook 'typescript-mode-hook #'my-tide-hook))

(defun my/start-impatient-mode ()
  (interactive)
  (httpd-start)
  (impatient-mode))

(use-package impatient-mode
  :general
  (leader-define
    :keymaps '(web-mode-map css-mode-map)
    "mit" 'impatient-mode
    "mis" 'my/start-impatient-mode
    "mik" 'httpd-stop))

;; clojure

(use-package clojure-mode)

(use-package cider)

;; kotlin

(use-package kotlin-mode)

(use-package gradle-mode)

(use-package flycheck-kotlin
  :config
  (flycheck-kotlin-setup))

;; lisp

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (add-hook 'clojure-mode-hook 'lspy-mode-hook))

;; Emacs lisp

(leader-define
  :keymaps 'emacs-lisp-mode-map
  "m" '(:ignore t :which-key "Emacs Lisp")
  "mb" 'eval-buffer
  "mr" 'eval-region
  "me" 'eval-expression
  "md" 'eval-defun)

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 "C-j" 'eval-region)

;; git

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :config
  (set-face-foreground 'git-gutter-fr+-modified "yellow")
  (set-face-foreground 'git-gutter-fr+-added    "green")
  (set-face-foreground 'git-gutter-fr+-deleted  "red")
  (global-git-gutter+-mode))

(use-package magit
  :general
  (leader-define
    "g" '(:ignore t :which-key "Git")
    "gs" 'magit-status
    "gd" 'magit-diff-dwim
    "gb" 'magit-blame
    "gl" 'magit-log
    "gr" 'magit-branch
    "gm" 'magit-merge)
  :ensure
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :ensure
  :config
  (evil-magit-init))

(use-package gitignore-mode)

;; company

(use-package company
  :ensure
  :diminish company-mode
  :general (:keymaps 'company-active-map
                     "<tab>" 'company-complete-common-or-cycle
                     "TAB" 'company-complete-common-or-cycle
                     "<backtab>" 'company-select-previous)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-limit 20)

  (use-package company-shell)
  (use-package company-cmake)

  (add-hook 'prog-mode-hook 'company-mode)

  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-capf company-yasnippet
                                       company-elisp company-files
                                       company-shell company-cmake))))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

(use-package company-flx
  :ensure
  :config
  (company-flx-mode))

;; eglot

(use-package eglot
  :general
  (leader-define
    :keymaps 'eglot-mode-map
    "er" 'eglot-rename
    "ef" 'eglot-format)
  :ensure
  :init
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  :config
  (setq eglot-autoshutdown t))

;; flycheck

(use-package flycheck
  :ensure
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 30)
  (flycheck-pos-tip-mode))

;; projectile

(use-package projectile
  :general
  (leader-define
    "p" '(:ignore t :which-key "Projectile")
    "pf" 'projectile-find-file
    "pb" 'projectile-switch-to-buffer
    "po" 'projectile-find-other-file
    "pk" 'projectile-kill-buffers
    "pt" 'projectile-run-term)
  :ensure
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

;; snippets

(use-package yasnippet
  :ensure
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; programming

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

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-visualizer-timestamps t))

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(use-package editorconfig
  :ensure
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

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
  :commands comment-tags-mode
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

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :config
  (add-hook 'prog-mode-hook 'eldoc-mode))

(defun trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'trailing-whitespace)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; org

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

(use-package ox-reveal)

;; writing

(use-package flyspell
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'latex-mode-hook 'flyspell-mode)
  (add-hook 'tex-mode-hook 'flyspell-mode))

(use-package flyspell-correct-ivy
  :general
  (:keymaps 'flyspell-mode-map
            :states '(normal visual)
            "z=" 'flyspell-correct-word-generic)
  (:keymaps 'flyspell-mode-map
            "C-;" 'flyspell-correct-word-generic))

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

(use-package tex
  :ensure nil)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

;; config files

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package nginx-mode
  :config
  (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

;; IRC

(use-package erc
  :ensure nil
  :commands erc
  :config
  (setq erc-log-channels-directory "~/.erc/logs/"
        erc-interpret-mirc-color t
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t
        erc-join-buffer 'bury
        erc-rename-buffers t
        erc-timestamp-format "[%H:%M] "
        erc-fill-column 120
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'spelling)
  (use-package erc-hl-nicks
    :ensure
    :config
    (erc-hl-nicks-enable))
  (erc-track-mode)
  (erc-truncate-mode)
  (erc-update-modules))

(defun my/erc-run ()
  (interactive)
  (if (get-buffer "freenode")
      (erc-track-switch-buffer)
    (erc :server "endoffile.org" :port 2000)))

(general-define-key "C-c i" 'my/erc-run)

;; fun

(use-package zone-nyan)

(use-package twittering-mode)

(use-package emojify
  :config
  (add-hook 'text-mode-hook 'emojify-mode)
  (add-hook 'org-mode-hook 'emojify-mode))

;; Extra configuration files

(defvar private-file (concat user-emacs-directory "private.el")
  "Private file that is not tracked.")
(defvar local-file (concat user-emacs-directory "local.el")
  "Local file specific to each computer.")

(if (file-exists-p private-file)
    (load private-file))

(if (file-exists-p local-file)
    (load local-file))

(load custom-file)

(provide 'init)
;;; init.el ends here
