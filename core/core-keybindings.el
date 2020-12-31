;;; core-keybindings.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;;  description
;;
;;; Code:

(defvar core/leader-key "SPC")
(defvar core/local-leader-key "SPC m")

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-setup-side-window-bottom)

  (which-key-add-key-based-replacements core/leader-key "<leader>")
  (which-key-add-key-based-replacements core/local-leader-key "<localleader>")

  (which-key-mode))

(use-package general
  :ensure
  :config
  (general-override-mode 1)
  (general-create-definer leader-def
    :states '(normal operator visual insert emacs)
    :prefix core/leader-key
    :non-normal-prefix "C-SPC")
  (general-create-definer local-leader-def
    :states '(normal operator visual insert emacs)
    :prefix core/local-leader-key
    :non-prefix "C-SPC m")

  (leader-def
   :states 'normal
   "" nil

   "c" (general-simulate-key "C-c")
   "x" (general-simulate-key "C-x")
   "h" (general-simulate-key "C-h")
   "u" (general-simulate-key "C-u")

   "b" '(:ignore t :which-key "Buffer Management")
   "bb" 'switch-to-buffer
   "bB" 'ibuffer-other-window
   "bk" 'kill-this-buffer
   "bK" 'kill-buffer

   "f" '(:ignore t :which-key "File")
   "ff" 'find-file
   "fi" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
   "fp" '(lambda () (interactive) (find-file core/private-file))
   "fl" '(lambda () (interactive) (find-file core/local-file))
   "fc" '(lambda () (interactive) (find-file custom-file))

   "t" '(:ignore t :which-key "Themes")
   "tc" 'my/cycle-theme

   "o" '(:ignore t :which-key "Open")
   "ot" 'my/open-term
   "oa" 'org-agenda
   "oc" 'org-capture

   ;; window management
   "w" '(:ignore t :which-key "Window Management")
   ;; window creating/deleting
   "w0" 'delete-window
   "w1" 'delete-other-windows
   "wu" 'winner-undo
   "wr" 'winner-redo
   "ws" 'evil-window-split
   "wv" 'evil-window-vsplit

   ;; window moving
   "ww" 'evil-window-new
   "wW" 'evil-window-prev
   "wo" 'other-window
   "wh" 'windmove-left
   "wj" 'windmove-down
   "wk" 'windmove-up
   "wl" 'windmove-right

   "w=" 'balance-windows)

  (general-define-key
   :states '(insert emacs)
   "C-a" 'beginning-of-line
   "C-e" 'end-of-line))

(provide 'core-keybindings)
;;; core-keybindings.el ends here
