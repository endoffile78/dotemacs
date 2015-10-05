;;; init-smartparens.el --- Smartparens configuration

(defun my-php-get-function-args (&optional name)
	"Return all arguments of php function.
	Point should be at the line containing `function'."
	(save-excursion
	(when name
		(goto-char (point-min))
		(unless (search-forward (concat "function " name) nil t)
		(error "Function %s does not exist" name)))
	(let ((function-args (sp-get (sp-down-sexp)
							(buffer-substring-no-properties :beg :end)))
			(args nil))
		(save-match-data
		(with-temp-buffer
			(insert function-args)
			(goto-char (point-min))
			(while (re-search-forward "\\(&?\\$.*?\\)[ \n\t,)]" nil t)
			(push (match-string 1) args))))
		(nreverse args))))

(defun my-php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ((string-match-p "function" line)
      (save-excursion
        (insert "\n")
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args))))
          (--each args
            (insert (format "* @param %s\n" it)))))
      (insert "* "))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n*\n* @author\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))


(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (sp-local-pair '(c++-mode c-mode)"/*" "*/" :post-handlers '((" | " "SPC")
															("* ||\n[i]" "RET")))
  (sp-with-modes '(php-mode)
	(sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                             (my-php-handle-docstring "RET")))
	(sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
	(sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

  (smartparens-global-mode t))

(provide 'init-smartparens)
