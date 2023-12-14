;; gptel
(use-package gptel
  :config
  (setq gptel-api-key (lambda () (getenv "OPENAIKEY"))))

(define-key dkj-keys (kbd "C-<return>") #'gptel-send)
