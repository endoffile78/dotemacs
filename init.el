;;; init.el --- Emacs configuration

;;; Commentary:
;;; My Emacs config

;;; Code:

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Monaco-11"))

(unless '(packge-installed-p 'use-package) ;; Make sure use-package is installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'cl))
(require 'diminish)
(require 'bind-key)

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

(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(defvar private-file)

(setq ring-bell-function 'ignore
	  browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "firefox"
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
	  private-file (concat user-emacs-directory "private.el"))

(setq-default truncate-lines 1
			  backward-delete-function nil
			  dired-listing-switches "-alhv"
			  dired-recursive-copies 'always
			  indent-tabs-mode t
			  tab-width 4)

;; Hydra

(use-package hydra
  :ensure)

;; Theme

(use-package darkokai-theme
  :ensure
  :init
  (setq darkokai-mode-line-padding 1)
  :config
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
				((evil-insert-state-p) '("#555555" . "#FFFFFF"))
				((evil-visual-state-p) '("#AB7EFF" . "#000000"))
				((evil-normal-state-p) '("#35393B" . "#FFFFFF"))
				((evil-emacs-state-p) '("#FF6159" . "#FFFFFF")))))
	(set-face-background 'mode-line (car color))
	(set-face-foreground 'mode-line (cdr color))))

(use-package evil
  :ensure
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

  (global-unset-key (kbd "C-w"))
  (global-set-key (kbd "C-w <right>") 'evil-window-right)
  (global-set-key (kbd "C-w <left>") 'evil-window-left)
  (global-set-key (kbd "C-w <down>") 'evil-window-down)
  (global-set-key (kbd "C-w <up>") 'evil-window-up)

  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "Q" 'evil-quit)

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
	  "k" 'kill-this-buffer
	  "l" 'load-file
	  "b" 'helm-buffers-list
	  "pf" 'helm-projectile
	  "ps" 'helm-projectile-switch-project
	  "pb" 'helm-projectile-switch-to-buffer
	  "pd" 'helm-projectile-find-dir
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
	  "c" 'compile
	  "t" 'elscreen-create
	  "db" 'gud-gdb
	  "h" 'split-window-horizontally
	  "v" 'split-window-vertically
	  "fs" 'flyspell-mode
	  "fp" 'flyspell-prog-mode
	  "ar" 'anaconda-mode-find-references
	  "ad" 'anaconda-mode-find-definitions
	  "aa" 'anaconda-mode-find-assignments
	  "ad" 'anaconda-mode-show-doc
	  "iu" 'insert-char
	  "mw" 'helm-man-woman
	  "dp" 'sp-unwrap-sexp))

  (use-package vimish-fold
	:defer 3
	:config
	(vimish-fold-global-mode 1)
	(use-package evil-vimish-fold
	  :diminish evil-vimish-fold-mode
	  :config
	  (evil-vimish-fold-mode)))

  (use-package evil-org
	:defer 2
	:diminish evil-org-mode)

  (evil-mode 1))

;; Flycheck

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package flycheck-irony
	:init
	(eval-after-load 'flycheck
	  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  :config
  (defhydra hydra-flycheck ()
	"Flycheck"
	("l" flycheck-list-errors "list errors" :exit t)
	("c" flycheck-check-buffer "check buffer" :exit t)
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
		 ("C-c h m" . helm-man-woman))
  :init
  (setq helm-quick-update t
		helm-bookmark-show-location t
		helm-M-x-fuzzy-match t
		helm-buffers-fuzzy-matching t)
  :config
  (use-package helm-flx
	:config
	(helm-flx-mode +1))
  (use-package helm-descbinds
	:bind (("C-h b" . helm-descbinds))
	:config
	(setq helm-descbinds-window-style 'split-window)
	(helm-descbinds-mode))
  (helm-mode 1))

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
  (setq indent-tabs-mode t
		c-default-style "my-c-style")
  (c-set-style "my-c-style"))

(add-hook 'c-mode-hook 'my-c-hook)
(add-hook 'c++-mode-hook 'my-c-hook)

;; ggtags

(use-package ggtags
  :diminish ggtags-mode
  :commands ggtags-mode
  :init
  (add-hook 'c-mode-common-hook
			(lambda ()
			  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
				(ggtags-mode 1)))))

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
  (use-package irony-eldoc)

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'irony-eldoc))

;; Company

(use-package company
  :ensure
  :diminish company-mode
  :init
  (setq company-idle-delay 0
		company-minimum-prefix-length 2
		company-tooltip-limit 20)
  :config
  (use-package company-irony)
  (use-package company-irony-c-headers)
  (use-package company-shell)
  (use-package company-cmake)
  (use-package company-jedi)

  (use-package company-quickhelp
	:config
	(company-quickhelp-mode 1))

  (add-hook 'after-init-hook 'global-company-mode)

  (eval-after-load 'company
	'(add-to-list
	  'company-backends '(company-irony company-irony-c-headers company-yasnippet
										company-css company-elisp company-semantic
										company-files company-shell
										company-cmake company-jedi)))

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

;; Java

(use-package java-file-create
  :defer 4)

(use-package jdee
  :commands jdee-mode)

;; LaTeX

(use-package tex
  :defer 3)

;; Markdown

(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

;; Vim

(use-package vimrc-mode
  :mode (".vim\\(rc\\)?$" . vimrc-mode))

;; Programming Utilities

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'makefile-mode)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode 'c++-mode 'java-mode 'csharp-mode)
		 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
							 (thing-at-point 'line)))))
  (global-aggressive-indent-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
		uniquify-separator "/"
		uniquify-ignore-buffers-re "^\\*"
		uniquify-after-kill-buffer-p t))

(use-package smart-comment
  :bind("M-;" . smart-comment))

;; Projectile

(defun my-projectile-hook ()
  "Check to see if the project is in a git repo or not and then set the indexing method."
  (let ((vcs (projectile-project-vcs)))
	(cond
	 ((eq vcs 'git) (setq projectile-indexing-method 'alien ;; Use .gitignore
						  projectile-enable-caching nil))
	 (t (setq projectile-indexing-method 'native ;; Use .projectile
			  projectile-enable-caching t)))))

(use-package projectile
  :ensure
  :config
  (add-hook 'projectile-before-switch-project-hook 'my-projectile-hook)
  (projectile-global-mode)
  (use-package helm-projectile
	:ensure
	:config
	(helm-projectile-on)))

;; Python

(defun python-f5 ()
  "Sends the buffer to a python shell."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "<f5>") 'python-f5)))

;; Smartparens

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (use-package evil-smartparens
	:diminish evil-smartparens-mode
	:init
	(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  (setq sp-show-pair-delay 0
		sp-show-pair-from-inside t)
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode)
  (smartparens-global-mode t)
  (defhydra hydra-smartparens ()
	"Smartparens"
	("f" sp-forward-sexp "forward")
	("b" sp-backward-sexp "backward")
	("u" sp-unwrap-sexp "unwrap" :exit t)
	("s" sp-show-enclosing-pair "show" :exit t)
	("q" nil "quit"))
  (global-set-key (kbd "C-c s") 'hydra-smartparens/body))

;; Visual

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
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
	("d" yas-load-direcoty "load directory")
	("q" nil "quit"))
  (global-set-key (kbd "C-c y") 'hydra-yasnippet/body))

;; Org

(use-package org
  :config
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

;; Emacs Lisp

(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'eval-buffer)

;; undo-tree

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (defhydra hydra-undo-tree ()
	"undo tree"
	("v" undo-tree-visualize "visualize" :exit t)
	("u" undo-tree-undo "undo")
	("r" undo-tree-redo "redo")
	("q" nil "quit"))
  (global-set-key (kbd "C-c u") 'hydra-undo-tree/body))

;; Abbrev

(use-package abbrev
  :diminish abbrev-mode)

;; ibuffer

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

;; elscreen

(use-package elscreen
  :demand t
  :bind (("C-c t" . elscreen-create)
		 ("C-c k" . elscreen-kill)
		 ("C-c n" . elscreen-next)
		 ("C-c b" . elscreen-previous)))

;; Lua

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

;; C#

(use-package csharp-mode
  :mode ("\\.cs$" . csharp-mode))

;; Font

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-frame-font "Monaco-11")

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
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'latex-mode-hook 'flyspell-mode)
  :config
  ;; NO spell check for embedded snippets
  (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
	(let ((rlt ad-return-value)
		  (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
		  (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
		  old-flag
		  b e)
	  (when ad-return-value
		(save-excursion
		  (setq old-flag case-fold-search)
		  (setq case-fold-search t)
		  (setq b (re-search-backward begin-regexp nil t))
		  (if b (setq e (re-search-forward end-regexp nil t)))
		  (setq case-fold-search old-flag))
		(if (and b e (< (point) e)) (setq rlt nil)))
	  (setq ad-return-value rlt))))

(use-package visual-line-mode
  :init
  (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'latex-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook 'visual-line-mode))

;; YAML

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; Haskell

(use-package haskell-mode)

;; Misc

(use-package fancy-battery-mode
  :init
  (add-hook 'after-init-hook #'fancy-battery-mode))

(use-package real-auto-save
  :diminish real-auto-save-mode
  :init
  (add-hook 'prog-mode-hook 'real-auto-save-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(fset 'yes-or-no-p 'y-or-n-p)

(defhydra hydra-scale ()
  "Scale"
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset" :exit t)
  ("q" nil "quit"))
(global-set-key (kbd "C-c z") 'hydra-scale/body)

(load custom-file)

(if (file-exists-p private-file)
	(load private-file))

(provide 'init)
;;; init.el ends here
