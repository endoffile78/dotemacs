(require 'company)
(require 'company-irony)
(require 'company-jedi)
(require 'company-irony-c-headers)

(setq company-idle-delay 0
	  company-minimum-prefix-length 2)

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
	 '(add-to-list
		   'company-backends '(company-irony-c-headers company-irony company-jedi company-yasnippet company-css)))

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

(provide 'init-company)
