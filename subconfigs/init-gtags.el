;;; init-gtags.el --- Configuration for ggtags

(use-package ggtags
  :diminish ggtags-mode
  :commands ggtags-mode
  :init
  (add-hook 'c-mode-common-hook
			(lambda ()
			  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
				(ggtags-mode 1)))))

(provide 'init-gtags)
