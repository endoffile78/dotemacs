(use-package smartparens-config
  :ensure smartparens
  :config
  (sp-with-modes
      '(c++-mode objc-mode c-mode)
    (sp-local-pair "/*" "*/" :post-handlers
                   '(:add
                     ("* [i]|\n[i]" newline evil-ret)
                     (" " c-context-line-break c-indent-new-comment-line)))
    (sp-local-pair "<" ">"))
  (sp-with-modes
      '(c++-mode objc-mode c-mode css-mode js2-mode web-mode java-mode rust-mode)
    (sp-local-pair "{" nil :post-handlers
                   '(:add
                     ("||\n[i]" "RET")
                     ("| " "SPC"))))
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-escape-quotes-after-insert nil)
  (sp-use-paredit-bindings)
  (smartparens-global-mode))

(use-package eglot
  :ensure
  :commands eglot-ensure
  :init
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  :config
  (setq eglot-autoshutdown t))

(provide 'mod-programming)
;;; mod-programming.el end here
