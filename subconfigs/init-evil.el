(require 'evil)
(require 'evil-leader)
(require 'evil-tabs)
(require 'evil-surround)

(global-evil-tabs-mode t)

(setq evil-normal-state-cursor '("white" box)
	  evil-insert-state-cursor '("red" bar))

(evil-leader/set-leader ",")
(global-evil-leader-mode)
(evil-leader/set-key "mg" 'mpd-get-current-song
					 "mc" 'mpd-clear-playlist
					 "k" 'kill-buffer
					 "l" 'load-file
					 "b" 'helm-buffers-list
					 "fn" 'flycheck-next-error
					 "fp" 'flycheck-previous-error
					 "fe" 'flycheck-list-errors
					 "fc" 'flycheck-buffer
					 "pf" 'helm-projectile
					 "mn" 'mpd-next
					 "mp" 'mpd-prev
					 "mp" 'mpd-pause
					 "gc" 'ggtags-create-tags
					 "gu" 'ggtags-update-tags
					 "gf" 'ggtags-find-file
					 "gs" 'ggtags-find-other-symbol)

(defun my-evil-modeline-change (default-color)
	"Changes the modeline color when the evil mode changes"
	(let ((color (cond
				  ((evil-insert-state-p) '("#FFFFFF" . "#000000"))
				  ((evil-visual-state-p) '("#330022" . "#FFFFFF"))
				  ((evil-normal-state-p) '("#000000" . "#FFFFFF"))
				  ((evil-emacs-state-p) ' ("#440000" . "#ffffff")))))
	(set-face-background 'mode-line (car color))
	(set-face-foreground 'mode-line (cdr color))))

(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
	(add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

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

(evil-ex-define-cmd "q" 'vimlike-quit)
(evil-ex-define-cmd "wq" 'vimlike-write-quit)

(global-evil-surround-mode 1)

(evil-mode 1)

(provide 'init-evil)
