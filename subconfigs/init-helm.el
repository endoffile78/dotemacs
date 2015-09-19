(require 'helm)
(require 'helm-projectile)

(setq helm-command-prefix-key "C-c h"
	  helm-quick-update t
	  helm-bookmark-show-location t
	  helm-M-x-fuzzy-match t
	  helm-buffers-fuzzy-matching t)

(helm-projectile-on)
(helm-mode 1)

(provide 'init-helm)
