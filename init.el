;;  -*- lexical-binding: t; -*-
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

(when (version< emacs-version "27" )
  (package-initialize))

(if (eq (system-name) 'gnu/linux)
    (progn
      (add-to-list 'exec-path "~/bin")
      (add-to-list 'exec-path "/usr/local/bin")))

(unless (package-installed-p 'use-package) ;; Make sure use-package is installed
  (package-refresh-contents)
  (package-install 'diminish)
  (package-install 'use-package))

;; functions

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (delete-window (selected-window)))

(defun my/open-term ()
  (interactive)
  (let ((w (split-window-below)))
    (select-window w)
    (term shell-file-name))
  (switch-to-buffer "*terminal*"))

(eval-when-compile
  (require 'use-package))
(require 'diminish)

;; general config

(setf epa-pinentry-mode 'loopback)

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
      default-directory (getenv "HOME")
      make-pointer-invisible t
      whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark))
      display-line-numbers-type 'relative)

(setq-default truncate-lines 1
              backward-delete-function nil
              indent-tabs-mode nil
              tab-width 4
              require-final-newline t)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(fset 'yes-or-no-p 'y-or-n-p)

(define-global-minor-mode my-global-display-line-numbers-mode global-display-line-numbers-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'eshell-mode 'calendar-mode 'term-mode
                           'doc-view-mode 'erc-mode 'shell-mode
                           'compilation-mode 'cargo-process-mode)))
      (display-line-numbers-mode))))

(my-global-display-line-numbers-mode)
(global-hl-line-mode 1)
(column-number-mode t)
(recentf-mode)
(blink-cursor-mode 1)
(toggle-save-place-globally)
(show-paren-mode 1)
(delete-selection-mode)
(winner-mode)
(size-indication-mode)

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
(add-to-list 'default-frame-alist '(font . "Iosevka-12"))

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

(use-package telephone-line
  :ensure
  :config
  (setq telephone-line-height 27
        telephone-line-evil-use-short-tag t)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-filesize-segment
                     telephone-line-projectile-buffer-segment))
          (nil    . (telephone-line-nyan-segment))
          (accent . (telephone-line-major-mode-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-flycheck-segment
                     telephone-line-atom-encoding-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode))

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package rainbow-delimiters
  :ensure
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; keybindings

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;; evil

(use-package evil
  :ensure
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; disable for evil-collection
  :config
  (setq evil-normal-state-cursor '("white" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
  (setq evil-visual-state-cursor '("purple" box))
  (setq evil-replace-state-cursor '("red" hbar))
  (setq evil-motion-state-cursor '("orange" box))

  (evil-set-leader '(normal visual replace motion operator) (kbd "SPC"))
  (evil-set-leader '(normal visual replace motion operator) (kbd ",") t)
  (evil-set-leader '(emacs insert) (kbd "C-SPC"))
  (evil-set-leader '(emacs insert) (kbd "C-,") t)

  (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bB") 'ibuffer-other-window)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bK") 'kill-buffer)

  (evil-define-key 'normal 'global (kbd "<leader>d") 'dired)

  (evil-define-key 'normal 'global (kbd "<leader>oa") 'org-agenda)
  (evil-define-key 'normal 'global (kbd "<leader>oc") 'org-capture)

  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fi") '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
  (evil-define-key 'normal 'global (kbd "<leader>fp") '(lambda () (interactive) (find-file "~/.emacs.d/private.el")))
  (evil-define-key 'normal 'global (kbd "<leader>fl") '(lambda () (interactive) (find-file "~/.emacs.d/local.el")))

  (evil-define-key 'normal 'global (kbd "<leader>tc") 'my/cycle-theme)

  (evil-define-key 'normal 'global (kbd "<leader>s") 'my/open-term)

  (evil-define-key 'normal 'global (kbd "<leader>w0") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>w1") 'delete-other-windows)
  (evil-define-key 'normal 'global (kbd "<leader>wu") 'winner-undo)
  (evil-define-key 'normal 'global (kbd "<leader>wr") 'winner-redo)
  (evil-define-key 'normal 'global (kbd "<leader>wo") 'other-window)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'windmove-left)
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'windmove-down)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'windmove-up)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'windmove-right)
  (evil-define-key 'normal 'global (kbd "<leader>wx") 'window-swap-states)
  (evil-define-key 'normal 'global (kbd "<leader>ws") 'evil-window-split)
  (evil-define-key 'normal 'global (kbd "<leader>wv") 'evil-window-vsplit)

  (evil-define-key 'normal 'global (kbd "C-a") 'beginning-of-line)
  (evil-define-key 'normal 'global (kbd "C-e") 'end-of-line)

  ;; C/C++
  (evil-define-key 'normal c-mode-map (kbd "<localleader>c") 'compile)
  (evil-define-key 'normal c-mode-map (kbd "<localleader>d") 'gud-gdb)
  (evil-define-key 'normal c++-mode-map (kbd "<localleader>c") 'compile)
  (evil-define-key 'normal c++-mode-map (kbd "<localleader>d") 'gud-gdb)

  (evil-set-initial-state 'snake-mode 'emacs)
  (evil-set-initial-state 'stacktrace-mode 'emacs)

  (evil-set-initial-state 'term-mode 'insert)
  (evil-set-initial-state 'shell-mode 'insert)
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'erc-mode 'insert)

  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "Q" 'evil-quit)

  (modify-syntax-entry ?_ "w")

  (evil-mode 1))

(use-package evil-collection
  :ensure
  :after evil
  :demand t
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

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
  :ensure
  :config
  (evil-define-key '(normal visual motion operator) 'global (kbd "f") 'evilem-motion-find-char)
  (evil-define-key '(normal visual motion operator) 'global (kbd "F") 'evilem-motion-find-char-backward)
  (evil-define-key '(normal visual motion operator) 'global (kbd "t") 'evilem-motion-find-char-to)
  (evil-define-key '(normal visual motion operator) 'global (kbd "T") 'evilem-motion-find-char-to-backward)
  (evil-define-key '(normal visual motion operator) 'global (kbd "(") 'evilem-motion-backward-sentence-begin)
  (evil-define-key '(normal visual motion operator) 'global (kbd ")") 'evilem-motion-forward-sentence-begin)
  (evil-define-key '(normal visual motion operator) 'global (kbd "<leader>j") 'evilem-motion-next-line)
  (evil-define-key '(normal visual motion operator) 'global (kbd "<leader>k") 'evilem-motion-previous-line))

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

(use-package swiper
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
  :config
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'counsel-M-x))

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

(use-package clang-format+
  :ensure
  :commands clang-format+-mode
  :config
  (add-hook 'c-mode-hook 'clang-format+-mode)
  (add-hook 'c++-mode-hook 'clang-format+-mode))

(defun my-makefile-hook ()
  "Hook for `makefile-mode'."
  (setq-local indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'my-makefile-hook)

(use-package cmake-mode
  :ensure
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))


;; Go

(use-package go-mode
  :ensure
  :mode ("\\.go\\'" . go-mode)
  :commands go-mode
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
  :bind
  (:map python-mode-map
        ("<f5>" . python-f5)))

(use-package virtualenvwrapper
  :config
  (evil-define-key 'normal python-mode-map (kbd "<localleader>a") 'venv-workon)
  (evil-define-key 'normal python-mode-map (kbd "<localleader>d") 'venv-deactivate)
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package pip-requirements)

;; shell

(use-package powershell)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(if (eq (system-name) 'gnu/linux)
    (setq-default shell-file-name "/bin/zsh"))

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
  :commands web-mode
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
  :commands js2-mode
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
 :config
 (evil-define-key 'normal css-mode-map (kbd "<localleader>is") 'my/start-impatient-mode)
 (evil-define-key 'normal css-mode-map (kbd "<localleader>ik") 'httpd-stop))

;; lisp

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode))

;; Emacs lisp

(evil-define-key 'normal emacs-lisp-mode-map (kbd "<localleader>eb") 'eval-buffer)
(evil-define-key 'normal emacs-lisp-mode-map (kbd "<localleader>er") 'eval-region)
(evil-define-key 'normal emacs-lisp-mode-map (kbd "<localleader>ee") 'eval-expression)
(evil-define-key 'normal emacs-lisp-mode-map (kbd "<localleader>ed") 'eval-defun)

(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-region)

;; git

(when (display-graphic-p)
  (use-package git-gutter-fringe+
    :diminish git-gutter+-mode
    :config
    (set-face-foreground 'git-gutter-fr+-modified "yellow")
    (set-face-foreground 'git-gutter-fr+-added    "green")
    (set-face-foreground 'git-gutter-fr+-deleted  "red")
    (global-git-gutter+-mode)))

(use-package magit
  :ensure
  :config
  (evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status)
  (evil-define-key 'normal 'global (kbd "<leader>gd") 'magit-diff-dwim)
  (evil-define-key 'normal 'global (kbd "<leader>gb") 'magit-blame)
  (evil-define-key 'normal 'global (kbd "<leader>gl") 'magit-log)
  (evil-define-key 'normal 'global (kbd "<leader>gr") 'magit-branch)
  (evil-define-key 'normal 'global (kbd "<leader>gm") 'magit-merge)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :after magit
  :ensure
  :config
  (evil-magit-init))

(use-package gitignore-mode)

;; company

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

  (use-package company-shell)
  (use-package company-cmake)

  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

;; eglot

(use-package eglot
  :ensure
  :commands eglot-ensure
  :init
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  :config
  (setq eglot-autoshutdown t))

;; flycheck

(use-package flycheck
  :ensure
  :commands flycheck-mode
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
  :ensure
  :config
  (evil-define-key 'normal 'global (kbd "<leader>pf") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>pb") 'projectile-switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>po") 'projectile-find-other-file)
  (evil-define-key 'normal 'global (kbd "<leader>pk") 'projectile-kill-buffers)
  (evil-define-key 'normal 'global (kbd "<leader>pt") 'projectile-run-term)
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
      '(c++-mode objc-mode c-mode css-mode js2-mode web-mode java-mode rust-mode)
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

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :config
  (add-hook 'prog-mode-hook 'eldoc-mode))

(defun trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'trailing-whitespace)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package rg
  :config
  (evil-define-key 'normal 'global (kbd "<leader>/") 'rg)
  (rg-enable-default-bindings))

;; writing

(use-package flyspell
  :config
  (evil-define-key '(normal visual) 'flyspell-mode-map (kbd "z=") 'flyspell-correct-at-point)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'latex-mode-hook 'flyspell-mode)
  (add-hook 'tex-mode-hook 'flyspell-mode))

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'tex-mode-hook 'visual-line-mode)

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(defun my/set-fill-column ()
  (setq-local fill-column 72))

(add-hook 'text-mode-hook 'my/set-fill-column)
(add-hook 'prog-mode-hook 'my/set-fill-column)

(use-package tex
  :ensure nil)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(use-package markdown-mode
  :commands markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook 'my/set-fill-column)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-ispell company-files)))))

(use-package org
  :config
  (add-hook 'org-mode-hook 'my/set-fill-column)
  (setq org-directory "~/docs/org/"
        org-default-notes-file "~/docs/org/notes.org"
        org-agenda-files `(,org-directory)
        org-log-done t
        org-startup-indented t))

(use-package ox-reveal)

;; config files

(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; Rust

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook 'rust-enable-format-on-save))

(use-package cargo
  :config
  (evil-define-key 'normal rust-mode-map (kbd "<localleader>c") 'cargo-process-build)
  (evil-define-key 'normal rust-mode-map (kbd "<localleader>r") 'cargo-process-run)
  (evil-define-key 'normal rust-mode-map (kbd "<localleader>t") 'cargo-process-test))

;; fun

(use-package zone-nyan)

(use-package emojify
  :config
  (add-hook 'text-mode-hook 'emojify-mode)
  (add-hook 'markdown-mode-hook 'emojify-mode)
  (add-hook 'org-mode-hook 'emojify-mode)
  (add-hook 'erc-mode-hook 'emojify-mode))

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
