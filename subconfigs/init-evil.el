;;;init-evil.el --- Configuration for evil and other evil related packages

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

(use-package evil
  :ensure
  :preface
  (defun vimlike-quit ()
	"Vimlike ':q' behavior: close current window if there are split windows;
	otherwise, close current tab (elscreen)."
	(interactive)
	(let ((one-elscreen (elscreen-one-screen-p))
		  (one-window (one-window-p)))
	  (cond
		; if current tab has split windows in it, close the current live window
	   ((not one-window)
		(kill-this-buffer)
		(delete-window) ; delete the current window
		(balance-windows) ; balance remaining windows
		nil)
		; if there are multiple elscreens (tabs), close the current elscreen
	   ((not one-elscreen)
		(kill-this-buffer)
		(elscreen-kill)
		nil)
	   ; if there is only one elscreen, just try to quit (calling elscreen-kill
	   ; will not work, because elscreen-kill fails if there is only one
	   ; elscreen)
	   (one-elscreen
		(evil-quit)
		nil)
	   )))

  (defun vimlike-write-quit ()
	"Vimlike ':wq' behavior: write then close..."
	(interactive)
	(save-buffer)
	(vimlike-quit))
  :config
  (setq evil-normal-state-cursor '("white" box)
		evil-insert-state-cursor '("red" bar)
		evil-operator-state-cursor '("red" hollow))
  (evil-ex-define-cmd "q" 'vimlike-quit)
  (evil-ex-define-cmd "wq" 'vimlike-write-quit)
  (evil-mode 1))

(use-package evil-tabs
  :config
  (global-evil-tabs-mode t))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

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
	"fn" 'flycheck-next-error
	"fp" 'flycheck-previous-error
	"fe" 'flycheck-list-errors
	"fc" 'flycheck-buffer
	"pf" 'helm-projectile
	"ps" 'helm-projectile-switch-project
	"pb" 'helm-projectile-switch-to-buffer
	"pd" 'helm-projectile-find-dir
	"mn" 'mpd-next
	"mb" 'mpd-prev
	"mp" 'mpd-pause
	"gc" 'ggtags-create-tags
	"gu" 'ggtags-update-tags
	"gf" 'ggtags-find-file
	"gs" 'ggtags-find-other-symbol
	"ms" 'magit-status
	"md" 'magit-diff
	"mb" 'magit-blame-popup
	"ml" 'magit-log-popup
	"ed" 'elpy-goto-definition
	"er" 'elpy-refactor
	"es" 'elpy-shell-switch-to-shell
	"yn" 'yas-new-snippet
	"yr" 'yas-reload-all
	"c" 'compile))

(use-package vimish-fold
  :config
  (vimish-fold-global-mode 1)
  (use-package evil-vimish-fold))

(provide 'init-evil)
