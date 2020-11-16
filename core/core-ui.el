;;; core-ui.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/endoffile>
;; Maintainer:  <endoffile@localhost>
;; Created: October 30, 2020
;; Modified: October 30, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/endoffile/core-ui
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Iosevka-14"))

(use-package darkokai-theme
  :ensure
  :config
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai t))

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

(use-package nyan-mode
  :ensure
  :config
  (nyan-mode 1))

(setq display-buffer-alist
      '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . 0))
        ("\\*terminal\\*"
         (display-buffer-in-side-window)
         (window-height . 0.20)
         (side . bottom)
         (slot . 0))))

(provide 'core-ui)
;;; core-ui.el ends here
