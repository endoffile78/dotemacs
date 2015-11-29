;;; init.el --- Emacs configuration

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(unless '(packge-installed-p 'use-package) ;Make sure use-package is installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(global-linum-mode)
(global-hl-line-mode 1)
(column-number-mode t)

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1))

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
	  custom-file (concat user-emacs-directory "custom.el"))

(setq-default truncate-lines 1
			  backward-delete-function nil
			  dired-listing-switches "-alhv"
			  dired-recursive-copies 'always
			  indent-tabs-mode t
			  tab-width 4)

;; Hydra

(use-package hydra)

;; Theme

(use-package ujelly-theme
  :load-path "themes/ujelly")

;; Evil

(defgroup dotemacs-evil nil
  "Configuration options for evil-mode."
  :group 'dotemacs
  :prefix 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-state-minor-modes
  '(git-commit-mode magit-blame-mode eshell-mode calendar-mode)
  "List of minor modes that when active should switch to Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(cl-loop for mode in dotemacs-evil/emacs-state-minor-modes
         do (let ((hook (concat (symbol-name mode) "-hook")))
              (add-hook (intern hook) `(lambda ()
										 (if ,mode
                                             (evil-emacs-state)
                                           (evil-normal-state))))))

(use-package evil
  :ensure
  :config
  (defun my-evil-modeline-change (default-color)
	"Changes the modeline color when the evil mode changes"
	(let ((color (cond
				  ((evil-insert-state-p) '("#FFFFFF" . "#000000"))
				  ((evil-visual-state-p) '("#330022" . "#FFFFFF"))
				  ((evil-normal-state-p) '("#000000" . "#FFFFFF"))
				  ((evil-emacs-state-p) '("#440000" . "#ffffff")))))
	  (set-face-background 'mode-line (car color))
	  (set-face-foreground 'mode-line (cdr color))))

  (lexical-let ((default-color (cons (face-background 'mode-line)
									 (face-foreground 'mode-line))))
	(add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

  (setq evil-normal-state-cursor '("white" box) ;Change the cursor color and shape based on the state
		evil-insert-state-cursor '("red" bar)
		evil-operator-state-cursor '("red" hollow))

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
	  "mg" 'mpd-get-current-song
	  "mc" 'mpd-clear-playlist
	  "k" 'kill-this-buffer
	  "l" 'load-file
	  "b" 'helm-buffers-list
	  "pf" 'helm-projectile
	  "ps" 'helm-projectile-switch-project
	  "pb" 'helm-projectile-switch-to-buffer
	  "pd" 'helm-projectile-find-dir
	  "pi" 'projectile-invalidate-cache
	  "mn" 'mpd-next
	  "mb" 'mpd-prev
	  "mp" 'mpd-pause
	  "gc" 'ggtags-create-tags
	  "gu" 'ggtags-update-tags
	  "gf" 'ggtags-find-file
	  "gs" 'ggtags-find-other-symbol
	  "gt" 'ggtags-find-tag-dwim
	  "ms" 'magit-status
	  "md" 'magit-diff
	  "mb" 'magit-blame
	  "ml" 'magit-log-popup
	  "c" 'compile
	  "t" 'elscreen-create
	  "d" 'gud-gdb))

  (use-package vimish-fold
	:defer 3
	:config
	(vimish-fold-global-mode 1)
	(use-package evil-vimish-fold))

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
  :ensure
  :config
  (defhydra hydra-magit (:exit t)
	"Magit"
	("s" magit-status "status")
	("b" magit-blame "blame")
	("d" magit-diff "diff")
	("l" magit-log-popup "log")
	("b" magit-branch-manager "branch manager")
	("q" nil "quit"))
  (global-set-key (kbd "C-c m") 'hydra-magit/body))

;; Helm

(use-package helm
  :ensure
  :demand t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x))
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
  (helm-mode 1)
  (defhydra hydra-helm (:exit t)
	"Helm"
	("x" helm-M-x "M-x")
	("r" helm-recentf "recent file")
	("q" nil "quit"))
  (global-set-key (kbd "C-c h") 'hydra-helm/body))

;; C

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(c-add-style "my-c-style" '((c-continued-statement-offset 4)
							(c-tab-always-indent t)
							(c-toggle-hungry-state t)
							(c-set-offset 'inline-open '+
										  'block-open '+
										  'brace-list-open '+
										  'case-label '+)))

(defun my-c-hook ()
  (setq indent-tabs-mode t)
  (c-set-style "my-c-style"))

(add-hook 'c-mode-hook 'my-c-hook)

;; ggtags

(use-package ggtags
  :diminish ggtags-mode
  :commands ggtags-mode
  :init
  (add-hook 'c-mode-common-hook
			(lambda ()
			  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
				(ggtags-mode 1))))
  :config
  (defhydra hydra-ggtags (:exit t)
	"ggtags"
	("c" ggtags-create-tags "create tags")
	("u" ggtags-update-tags "update tags")
	("t" ggtags-find-tag-dwim "find tag")
	("f" ggtags-find-file "find file")
	("q" nil "quit"))
  (global-set-key (kbd "C-c g") 'hydra-ggtags/body))

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

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

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
  (use-package company-jedi)

  (add-hook 'after-init-hook 'global-company-mode)

  (eval-after-load 'company
	'(add-to-list
	  'company-backends '(company-irony-c-headers company-irony company-jedi company-yasnippet company-css company-elisp company-semantic company-files)))

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
  :commands agressive-indent-mode
  :init
  (add-hook 'prog-mode-hook 'aggressive-indent-mode))

(use-package expand-region
  :demand
  :bind (("C-=" . er/expand-region)
		 ("C--" . er/contract-region))
  :config
  (defhydra hydra-expand-region ()
	"Expand region"
	("e" er/expand-region "expand")
	("c" er/contract-region "contract")
	("w" er/mark-word "mark word" :exit t)
	("m" er/mark-method-call "mark method" :exit t)
	("s" er/mark-symbol "mark symbol" :exit t)
	("q" nil "quit"))
  (global-set-key (kbd "C-c r") 'hydra-expand-region/body))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
		uniquify-separator "/"
		uniquify-ignore-buffers-re "^\\*"
		uniquify-after-kill-buffer-p t))

;; Projectile

(use-package projectile
  :ensure
  :config
  (defun my-projectile-hook ()
	"Checks to see if project is in a git repo or not and then sets the indexing method"
	(let ((vcs (projectile-project-vcs)))
	  (cond
	   ((eq vcs 'git) (setq projectile-indexing-method 'alien ;Use .gitignore
							projectile-enable-caching nil))
	   (t (setq projectile-indexing-method 'native ;Use .projectile
				projectile-enable-caching t)))))
  (add-hook 'projectile-before-switch-project-hook 'my-projectile-hook)
  (projectile-global-mode)
  (use-package helm-projectile
	:ensure
	:config
	(helm-projectile-on)))

;; Python

(use-package elpy
  :commands elpy-enable
  :init
  (setq elpy-rpc-backend "jedi")
  (elpy-enable)
  :config
  (defhydra hydra-elpy (:exit t)
	"Elpy"
	("d" elyp-goto-definition "got definition")
	("r" elpy-refactor "refactor")
	("s" elpy-rgrep-symbol "find symbol")
	("f" elpy-find-file "find file")
	("m" elpy-multiedit "multiple cursors")
	("q" nil "quit"))
  (global-set-key (kbd "C-c e") 'hydra-elpy/body))

(use-package jedi
  :commands jedi:setup
  :init
  (add-hook 'python-mode-hook 'jedi:setup))

;; Smartparens

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (setq sp-show-pair-delay 0
		sp-show-pair-from-inside t)
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode)
  (smartparens-global-mode t))

;; Visual

(use-package nyan-mode
  :if window-system
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
  (add-hook 'css-mode-hook 'rainbow-mode))

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
		  web-mode-enable-block-face t)
	(rainbow-mode))
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

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c b") 'eval-buffer)

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

;; pretty-mode

(use-package pretty-mode
  :commands turn-on-pretty-mode
  :init
  (add-hook 'prog-mode-hook 'turn-on-pretty-mode))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

;; Misc

(use-package libmpdee
  :defer 1)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defhydra hydra-scale ()
  "Scale"
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset" :exit t)
  ("q" nil "Quit"))
(global-set-key (kbd "C-c s") 'hydra-scale/body)

(load custom-file)

(provide 'init)
