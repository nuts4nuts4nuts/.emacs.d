;; Hide extra bars
(when (eq system-type 'windows-nt)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1))

;; Add repos for the package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;;; Evil Mode things
;; Evil Leader on
(defun open-init ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (find-file "~/.emacs.d/init.el"))

(require 'evil-leader)
(global-evil-leader-mode)
    ;; leader configurations
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "f" 'helm-find-files
      "b" 'helm-buffers-list
      "k" 'kill-buffer
      "d" 'dired
      "in" 'open-init)

;; Evil Mode on
(require 'evil)
(evil-mode t)

;; ESC quits
(require 'evil-escape)
(evil-escape-mode t)
(setq-default evil-escape-key-sequence "kj")
(setq-default evil-escape-delay 0.2)

;; Scroll up and down
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
		    (interactive)
		    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
			(interactive)
			(evil-scroll-down nil)))
;;; Evil Mode things end

;; Unbind clipboard integration (so that I can use the VIM clipboard stuff
(setq x-select-enable-clipboard nil)

;; Powerline makes the info bar at the bottom real pretty
(require 'powerline)
(powerline-evil-center-color-theme)

;; Helm is the craziest thing ever
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Zenburn theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("8e66467d0701835107f3f7e301d9be9739c40d9f63e0c65695870dc6ab01c564" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "f9bb8302f2fc463c68df5951fbcff1bb4ea88aa96080ea94d808cafc05d956b0" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Display tooltips in the echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Disable splash screen
(setq inhibit-splash-screen t)

;; M-j is indent-new-comment-line  
;; M-k is kill-sentence                       ; REBIND THESE THINGS MAYBE
;; M-h is mark-paragraph
;; M-l is downcase-word
;; Took the i3 switch window bindings + normal C-tab thing
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "<C-tab>") 'other-window)

;; auto-complete-mode
(global-auto-complete-mode t)

;; TESTING OMNISHARP STUFF
(require 'omnisharp)
;; Example evil-mode config
;; ~~~~~NOTE: <SPC> does <Leader> stuff but not very elegantly. Mode mapping not working~~~~~
(evil-define-key 'insert omnisharp-mode-map (kbd "M-.") 'omnisharp-auto-complete)
(evil-define-key 'normal omnisharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)
(evil-define-key 'normal omnisharp-mode-map (kbd "gou") 'omnisharp-find-usages)
(evil-define-key 'normal omnisharp-mode-map (kbd "goi") 'omnisharp-find-implementations) ; g i is taken
(evil-define-key 'normal omnisharp-mode-map (kbd "god") 'Omnisharp-go-to-definition)
(evil-define-key 'normal omnisharp-mode-map (kbd "goR") 'omnisharp-run-code-action-refactoring)
(evil-define-key 'normal omnisharp-mode-map (kbd "gf") 'omnisharp-fix-code-issue-at-point)
(evil-define-key 'normal omnisharp-mode-map (kbd "gF") 'omnisharp-fix-usings)
(evil-define-key 'normal omnisharp-mode-map (kbd "gor") 'omnisharp-rename)
(evil-define-key 'normal omnisharp-mode-map (kbd ", i") 'omnisharp-current-type-information)
(evil-define-key 'normal omnisharp-mode-map (kbd ", I") 'omnisharp-current-type-documentation)
(evil-define-key 'insert omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
(evil-define-key 'normal omnisharp-mode-map (kbd ", n t") 'omnisharp-navigate-to-current-file-member)
(evil-define-key 'normal omnisharp-mode-map (kbd ", n s") 'omnisharp-navigate-to-solution-member)
(evil-define-key 'normal omnisharp-mode-map (kbd ", n f") 'omnisharp-navigate-to-solution-file-then-file-member)
(evil-define-key 'normal Omnisharp-mode-map (kbd ", n F") 'omnisharp-navigate-to-solution-file)
(evil-define-key 'normal omnisharp-mode-map (kbd ", n r") 'omnisharp-navigate-to-region)
(evil-define-key 'normal omnisharp-mode-map (kbd "<f12>") 'omnisharp-show-last-auto-complete-result)
(evil-define-key 'insert omnisharp-mode-map (kbd "<f12>") 'omnisharp-show-last-auto-complete-result)
(evil-define-key 'normal omnisharp-mode-map (kbd ",.") 'omnisharp-show-overloads-at-point)
(evil-define-key 'normal omnisharp-mode-map (kbd ",rl") 'recompile)

(evil-define-key 'normal omnisharp-mode-map (kbd ",rt")
  (lambda() (interactive) (omnisharp-unit-test "single")))

(evil-define-key 'normal omnisharp-mode-map
  (kbd ",rf")
  (lambda() (interactive) (omnisharp-unit-test "fixture")))

(evil-define-key 'normal omnisharp-mode-map
  (kbd ",ra")
  (lambda() (interactive) (omnisharp-unit-test "all")))

;; Speed up auto-complete on mono drastically. This comes with the
;; downside that documentation is impossible to fetch.
(setq omnisharp-auto-complete-want-documentation nil)

;; Set the curl path.
(setq omnisharp--curl-executable-path "~/curl/curl.exe")
(setq omnisharp-server-executable-path "~/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")

;; Automatically start omnisharp-emacs when editing csharp files.
(add-hook 'csharp-mode-hook 'omnisharp-mode)
;; END OF TESTING OMNISHARP STUFF
