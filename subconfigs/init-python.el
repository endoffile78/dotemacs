(require 'elpy)

;; Rebind some of the keys
(define-key elpy-mode-map (kbd "C-c t") 'elpy-goto-definition)
(define-key elpy-mode-map (kbd "C-c r") 'elpy-refactor)
(define-key elpy-mode-map (kbd "C-c d") 'elpy-doc)
(define-key elpy-mode-map (kbd "C-c e") 'elpy-multiedit-python-symbol-at-point)

(elpy-enable)
(highlight-indentation-mode 0)

(provide 'init-python)
