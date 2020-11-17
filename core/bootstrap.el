(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(add-to-list 'load-path (concat user-emacs-directory "core/os/"))

(cond
 (IS-WINDOWS
  (require 'windows))
 (IS-MAC
  (require 'osx))
 (IS-LINUX
  (require 'linux)))

(add-to-list 'load-path (concat user-emacs-directory "core/editor/"))

(defconst core/etc-dir (concat user-emacs-directory "etc/"))
(defconst core/cache-dir (concat core/etc-dir "cache/"))
(defconst core/private-dir (concat core/etc-dir "private/"))

(defconst core/local-file (concat core/private-dir "local.el"))
(defconst core/private-file (concat core/private-dir "private.el"))

(setq ring-bell-function 'ignore
      inhibit-startup-message t
      initial-scratch-message nil
      auto-save-default nil
      make-backup-files t
      delete-old-versions t
      initial-major-mode 'text-mode
      ad-redefinition-action 'accept
      custom-safe-themes t
      find-file-visit-truename t
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

(setf epa-pinentry-mode 'loopback)

;;; recentf
(setq recentf-max-saved-items 50)
(setq recentf-exclude '("/elpa/"
                        "/games/"))

;;; uniquify
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*"
      uniquify-after-kill-buffer-p t)

;;; dired
(setq dired-listing-switches "-alh"
      dired-recursive-copies 'always)

;; where to store things
(setq abbrev-file-name                   (concat core/etc-dir "abbrev.el")
      bookmark-default-file              (concat core/etc-dir "bookmarks")
      custom-file                        (concat core/etc-dir "custom.el")
      custom-theme-directory             (concat core/etc-dir "themes/")
      url-cache-directory                (concat core/cache-dir "url/")
      url-configuration-directory        (concat core/etc-dir "url/")
      gamegrid-user-score-file-directory (concat core/cache-dir "games/")
      recentf-save-file                  (concat core/cache-dir "recentf")
      auto-insert-directory              (concat core/etc-dir "auto-insert")
      ;;backup-directory-alist             '((".*" . (concat core/cache-dir "backups")))
      backup-directory-alist             '((".*" . "~/.emacs.d/etc/cache/backups/"))
      request-storage-directory          (concat core/etc-dir "request")
      save-place-file                    (concat core/cache-dir "places")
      auto-save-list-file-prefix         (concat core/cache-dir "auto-save-list/.saves-"))

(setq revert-without-query '(".*"))

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

;; programming

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(defun trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'trailing-whitespace)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun my/set-fill-column ()
  (setq-local fill-column 72))

(add-hook 'text-mode-hook 'my/set-fill-column)
(add-hook 'prog-mode-hook 'my/set-fill-column)
(add-hook 'org-mode-hook 'my/set-fill-column)

(defun core/init ()
  (require 'core-keybindings)
  (require 'editor-evil)
  (require 'core-packages)
  (require 'core-ui))

(defun core/load-private-configs ()
  (if (file-exists-p core/local-file)
      (load core/local-file))

  (if (file-exists-p core/private-file)
      (load core/private-file))

  (load custom-file))

(add-hook 'after-init-hook 'core/load-private-configs) ;; load private configs and the custom file after the init is loaded

(provide 'bootstrap)
;;; bootstrap.el ends here
