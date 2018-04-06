;; Hide extra bars
(when (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;;Don't make backup files
(setq make-backup-files nil)

;; Indent with 4 spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; C/C++ indentation settings
(setq c-default-style "bsd"
      c-basic-offset 'tab-width)

;; Open and focus this file: init.el
(defun open-init ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (find-file "~/.emacs.d/init.el"))

;; Open and focus my main org mode file
(defun open-org ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (dired "~/.emacs.d/org"))

;; Javascript settings
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

;; Add repos for the package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Helm is the craziest thing ever
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)

;; We're in good company now
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

;; moe~~~~
(require 'moe-theme)
(setq moe-theme-highlight-buffer-id t)
(moe-dark)
(moe-theme-set-color 'red)

;; Shows matching parens 
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0.01)

;; Display tooltips in the echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Magit is magic
(require 'magit)

;; If we're on windows, use git-gui
(when (eq system-type 'windows-nt)
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

;; Associate nasm-mode with .asm files
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))

;;("f" 'helm-find-files
;;"b" 'helm-buffers-list
;;"k" 'kill-buffer
;;"d" 'dired
;;"x" 'helm-M-x
;;"v" 'evil-window-vsplit
;;"h" 'evil-window-split
;;"in" 'open-init
;;"go" 'open-org
;;"gs" 'magit-status)

;; Clojure mode bindings
  ;;"e" 'cider-eval-last-sexp
  ;;"c" 'cider-load-buffer)

;; Org-mode settings
(setq org-log-done 'time)
(setq org-startup-truncated nil)
  ;;"," 'org-metaleft
  ;;"." 'org-metaright
  ;;"gj" 'org-metadown
  ;;"gk" 'org-metaup
  ;;"o" '(lambda ()
	 ;;(interactive)
	 ;;(evil-append-line 1)
	 ;;(org-meta-return)
	 ;;(evil-normal-state))
  ;;"gt" 'org-todo
  ;;"st" '(lambda ()
	  ;;(interactive)
	  ;;(evil-append-line 1)
	  ;;(insert-char ?\s 1)
	  ;;(org-timer)
	  ;;(evil-normal-state))
  ;;"gi" 'org-clock-in
  ;;"go" 'org-clock-out
  ;;"gl" 'org-open-at-point
  ;;"c" 'org-toggle-checkbox)
;;(evil-define-key 'insert org-mode-map (kbd "M-a") 'org-time-stamp) ; This was backward-sentence

;; M-j is indent-new-comment-line  
;; M-k is kill-sentence                       ; REBIND THESE THINGS MAYBE
;; M-h is mark-paragraph
;; M-l is downcase-word
;; Took the i3 switch window bindings + normal C-tab thing
;;(global-set-key (kbd "M-j") 'windmove-down)
;;(global-set-key (kbd "M-k") 'windmove-up)
;;(global-set-key (kbd "M-h") 'windmove-left)
;;(global-set-key (kbd "M-l") 'windmove-right)
;;(global-set-key (kbd "<C-tab>") 'other-window)

;; Company is cool
;;(define-key evil-normal-state-map (kbd "C-n") 'company-complete)
;;(define-key evil-insert-state-map (kbd "C-n") 'company-complete)

  ;;"ci" 'evilnc-comment-or-uncomment-lines)

;;;;;;;;;;;; SOME GUI VOODOO HERE ;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/.emacs.d/org/Schedule-Senior-1.org")))
(custom-set-faces))
