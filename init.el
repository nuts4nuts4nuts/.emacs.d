;; Hide extra bars
(when (eq system-type 'windows-nt)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1))

;; Unbind clipboard integration (so that I can use the VIM clipboard stuff.
(setq x-select-enable-clipboard nil)

;; IDO mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Smex on
(global-set-key (kbd "M-x") 'smex)

;; Use Marmalade repo
(require 'package)
(package-initialize)
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)

;; Evil Leader on
(require 'evil-leader)
(global-evil-leader-mode)
    ;; leader configurations
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "f" 'find-file
      "b" 'switch-to-buffer
      "k" 'kill-buffer
      "d" 'ido-dired)

;; Evil Mode on
(require 'evil)
(evil-mode 1)

;; ESC quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state); esc quits

;; Scroll up and down
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
		    (interactive)
		    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
			(interactive)
			(evil-scroll-down nil)))

;; Zenburn theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "f9bb8302f2fc463c68df5951fbcff1bb4ea88aa96080ea94d808cafc05d956b0" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make prompts nicer
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-confirm-create-new-buffer 'always)

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

;; Don't prompt for nonexistent buffer
(setq ido-create-new-buffer 'always)

;; auto-complete-mode
(global-auto-complete-mode 1)

;; Open my init file with <Leader>in
(defun open-init ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (find-file "~/.emacs.d/init.el"))
(evil-leader/set-key "in" 'open-init)

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
