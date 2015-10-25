;;; init-dired.el --- Dired settings 

;; Handle zip compression
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq-default dired-listing-switches "-alhv"
			  dired-recursive-copies 'always)

(provide 'init-dired)

