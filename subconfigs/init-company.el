(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

(use-package company
  :ensure
  :diminish company-mode
  :init
  (setq company-idle-delay 0
		company-minimum-prefix-length 2)
  :config
  (use-package company-irony)
  (use-package company-irony-c-headers)
  (use-package company-jedi)
  
  (add-hook 'after-init-hook 'global-company-mode)

  (eval-after-load 'company
	'(add-to-list
	  'company-backends '(company-irony-c-headers company-irony company-jedi company-yasnippet company-css company-elisp)))

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(provide 'init-company)
