;; gptel
(use-package gptel
  :config
  (setq gptel-api-key (lambda () (getenv "OPENAIKEY"))))

(define-key dkj-keys (kbd "C-<return>") #'gptel-send)

(setq org-agenda-files
      (cons "~/Documents/12lang/arielle-lang" org-agenda-files))
