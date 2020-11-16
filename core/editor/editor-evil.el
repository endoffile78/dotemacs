;;; evil.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/endoffile>
;; Maintainer:  <endoffile@localhost>
;; Created: October 24, 2020
;; Modified: October 24, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/endoffile/evil
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(use-package evil
  :ensure
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; disable for evil-collection
  (setq evil-undo-system 'undo-tree)
  :config
  (setq evil-normal-state-cursor '("white" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
  (setq evil-visual-state-cursor '("purple" box))
  (setq evil-replace-state-cursor '("red" hbar))
  (setq evil-motion-state-cursor '("orange" box))

  ;; C/C++
  ;;(evil-define-key 'normal c-mode-map (kbd "<localleader>c") 'compile)
  ;;(evil-define-key 'normal c-mode-map (kbd "<localleader>d") 'gud-gdb)
  ;;(evil-define-key 'normal c++-mode-map (kbd "<localleader>c") 'compile)
  ;;(evil-define-key 'normal c++-mode-map (kbd "<localleader>d") 'gud-gdb)

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

(use-package evil-surround
  :ensure
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :ensure
  :config
  (global-evil-visualstar-mode t))

(use-package evil-commentary
  :ensure
  :diminish evil-commentary-mode
  :config
  (add-hook 'prog-mode-hook 'evil-commentary-mode))

(use-package evil-smartparens
  :after smartparens
  :ensure
  :diminish evil-smartparens-mode
  :config
  (add-hook 'smartparens-mode-hook 'evil-smartparens-mode))

(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :ensure
    :config
    (evil-terminal-cursor-changer-activate)))

(use-package evil-snipe
  :ensure
  :config
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package undo-tree
  :ensure
  :diminish undo-tree-mode
  :config
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(provide 'editor-evil)
;;; evil.el ends here
