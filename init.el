(defun dkj/org-babel-tangle-config ()
  "Automatically tangle our config.org config file when we save it"
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dkj/org-babel-tangle-config)))

;; Turn off all the bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Except the tab bar ;)
(tab-bar-mode 1)

;; Always prompt before exiting
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; Turn off the beeping with visible-bell
(setq visible-bell t)

;; command is a lot more ergonomic than option
;; also, less confusing when going back and forth between mac and windows
(setq mac-command-modifier 'meta)

;; Allow the mouse in terminal mode
(xterm-mouse-mode 1)

;; I can't see a god damn at this small font size
(defun dkj/font-height (height)
  "Prompts the user for a height and sets the font height.
Uses the prefix arg if one is provided."
  (interactive "NHeight: ")
  (set-face-attribute 'default nil :height height))
(dkj/font-height 140)

;; Automatically set view-mode when in a readonly buffer
;; Set a buffer as readonly with C-x C-q
(setq view-read-only t)

;; Use bar cursor since it matches the emacs model better
(setq-default cursor-type 'bar)

;; Add more context when scrolling around
(setq next-screen-context-lines 10)

;; Move to the top or bottom of the buffer when scrolling
(setq scroll-error-top-bottom 1)

;; Bigger global mark ring
(setq global-mark-ring-max 50)

;; Show column 80
(setq fill-column 80)
(global-display-fill-column-indicator-mode)

;; Prefer vertical splits in more cases
(setq split-width-threshold 90)
(setq split-height-threshold 100)

;; Backup to the ~/.emacs.d/backups directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; When I send mail from emacs, open the default mail client (because I haven't set up sending mail from emacs yet).
(setq send-mail-function 'mailclient-send-it)

;; Put the menu bar in the tab bar, to help with discoverability
(defun dkj/tab-bar-format-menu-bar ()
"Produce the Menu button (denoted as λ) for the tab bar that shows the menu bar."
`((menu-bar menu-item (propertize "λ" 'face 'tab-bar-tab-inactive)
   tab-bar-menu-bar :help "Menu Bar")))
(add-to-list 'tab-bar-format #'dkj/tab-bar-format-menu-bar)

;; Winner mode remembers my window layouts
(winner-mode 1)

;; Save minibuffer history across sessions
(savehist-mode 1)

;; Automatically pull changes to files from disk
(global-auto-revert-mode 1)
;; Same for non-file buffers (like dired, maybe magit?)
(setq global-auto-revert-non-file-buffers t)

;; Default to lax whitespace in isearch and match any char
(setq search-whitespace-regexp ".*")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)

;; Reserve this for tmux. Previously toggle-input-method
(global-unset-key (kbd "C-\\"))

;; query replace with regexp by default
;; Swap bindings with plain query replace
(global-set-key (kbd "M-%") #'query-replace-regexp)
(global-set-key (kbd "C-M-%") #'query-replace)

;; Easier window movement
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-o") #'other-window)

;; Kill line backwards
(global-set-key (kbd "M-<backspace>") (lambda () (interactive) (kill-line 0)))

;; Use dwim versions of upcase and downcase instead of char/word/region-specific verions
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)

;; Use cycle-spacing since it replaces just-one-space,
;; delete-horizontal-space, and delete-blank-lines all in one
(global-set-key (kbd "M-SPC") #'cycle-spacing)

;; C-c h to open this file, my config
(defun dkj/open-config ()
  "Open this file"
  (interactive)
  (push-mark)
  (find-file "~/.emacs.d/README.org"))
(global-set-key (kbd "C-c h") #'dkj/open-config)

;; Pulse the line when making big movements
(defun dkj/pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
		   recenter-top-bottom other-window))
  (advice-add command :after #'dkj/pulse-line))

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(load-theme 'modus-vivendi t)

(use-package which-key
  :config
  (which-key-mode))

(use-package magit)

(when (not (display-graphic-p))
  (add-to-list 'package-archives
               '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
  (use-package term-keys
    :config
    (term-keys-mode t)))

(use-package markdown-mode)

(use-package racket-mode)

(setq org-directory "~/org/"
      org-agenda-files '("~/org/")
      org-id-locations-file "~/org/.org-id-locations"
      org-startup-truncated nil)

;; Make inserting new list items a little cleaner
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "M-<return>") #'org-insert-item))

(setq org-export-backends '(ascii html icalendar latex md odt))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   ))

(setq org-babel-python-command "python3")

;; Load customize stuff
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load Google stuff
(let ((googel (concat user-emacs-directory "google.el")))
  (when (file-exists-p googel)
    (load googel)))
