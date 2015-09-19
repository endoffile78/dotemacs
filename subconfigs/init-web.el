(require 'js2-mode)
(require 'php-mode)
(require 'php-extras)
(require 'web-mode)

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2
		web-mode-css-indent-offset 4
		web-mode-code-indent-offset 4
		web-mode-enable-auto-pairing t
		web-mode-enable-auto-closing t
		web-mode-style-padding 2
		web-mode-script-padding 2
		web-mode-enable-current-element-highlight t)
  (highlight-symbol-mode nil)
  (rainbow-mode) 
)

(set-face-attribute 'web-mode-html-tag-face nil :foreground "#fad07a")
(set-face-attribute 'web-mode-doctype-face nil :foreground "#8fbfdc")
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#ffffff")
(set-face-attribute 'web-mode-current-element-highlight-face nil :background "#808080")

(add-hook 'html-mode-hook
	(lambda ()
		(set (make-local-variable 'sgml-basic-offset) 2)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'php-mode-hook 'my-php-mode-hook)

(defun my-php-mode-hook ()
  (setq indent-tabs-mode t)
  (let ((my-tab-width 4))
    (setq tab-width my-tab-width
		  c-basic-indent my-tab-width)
    (set (make-local-variable 'tab-stop-list)
         (number-sequence my-tab-width 200 my-tab-width))))

(provide 'init-web)
