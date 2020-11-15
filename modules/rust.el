;;; rust.el --- description -*- lexical-binding: t; -*-
;;
;;; Code:

(use-package rust-mode
  :ensure
  :config
  (add-hook 'rust-mode-hook 'rust-enable-format-on-save))

(use-package cargo
  :ensure
  :general
  (local-leader-def
    :states 'normal
    :keymaps 'rust-mode-map
    "cb" 'cargo-process-build
    "cr" 'cargo-process-run
    "ct" 'cargo-process-test))

(provide 'rust)
;;; rust.el ends here
