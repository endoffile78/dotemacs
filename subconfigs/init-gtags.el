;;; init-gtags.el --- Configuration for ggtags

(use-package ggtags
  :diminish ggtags-mode
  :commands ggtags-mode
  :init
  (add-hook 'c-mode-common-hook
			(lambda ()
			  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
				(ggtags-mode 1))))
  :config
  (defhydra hydra-gtags (:exit t)
	("c" ggtags-create-tags "Create tags")
	("u" ggtags-update-tags "Update tags")
	("t" ggtags-find-tag-dwim "Find tag")
	("f" ggtags-find-file "Find file"))
  (global-set-key (kbd "C-c g") 'hydra-gtags/body))

(provide 'init-gtags)
