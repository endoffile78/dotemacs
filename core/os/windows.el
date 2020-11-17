;;; windows.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;;; Code:

(use-package powershell
  :ensure)

(setenv "HOME" (getenv "USERPROFILE"))

(provide 'windows)
;;; windows.el ends here
