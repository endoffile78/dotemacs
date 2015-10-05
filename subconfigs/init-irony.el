;;; init-irony.el --- Configuration for irony

(c-add-style "my-c-style" '((c-continued-statement-offset 4)
			    (c-set-offset 'inline-open '+
					  'block-open '+
					  'brace-list-open '+
					  'case-label '+)))

(use-package irony
  :ensure
  :commands irony-mode
  :preface  
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's asynchronous function
  (defun my-irony-mode-hook ()
	(define-key irony-mode-map [remap completion-at-point]
	  'irony-completion-at-point-async)
	(define-key irony-mode-map [remap complete-symbol]
	  'irony-completion-at-point-async)
	(setq indent-tabs-mode t)
	(c-set-style "my-c-style"))
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'my-irony-mode-hook))

(provide 'init-irony)
