(require 'ibuffer)

;; Handle zip compression
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq-default dired-listing-switches "-alhv")
(setq dired-recursive-copies 'always)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))

(provide 'init-dired)

