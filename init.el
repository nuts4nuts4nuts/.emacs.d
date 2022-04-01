;; command is a lot more ergonomic than option
;; also, less confusing when going back and forth between mac and windows
(setq mac-command-modifier 'meta)

;; C-c h to open this file, my config
(defun dkj/open-config ()
  "Open this file"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c h") 'dkj/open-config)

;; Not having equivalents to Vim o/O is killing me
(defun dkj/open-next-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "C-o") 'dkj/open-next-line)

(defun dkj/open-previous-line ()
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "C-S-o") 'dkj/open-previous-line)

;; Automatically support view-mode when we're in readonly-mode. From here: https://karthinks.com/software/batteries-included-with-emacs/#view-mode--m-x-view-mode
(setq view-read-only t)

;; Use the modus-vivendi theme - https://protesilaos.com/emacs/modus-themes
(load-theme 'modus-vivendi)

;; Improve minibuffer completion
(setq completion-styles '(partial-completion flex)) ; > Emacs 27.1
(setq completion-cycle-threshold 3)

;; Turn off the beeping with visible-bell
(setq visible-bell t)

;; Turn off all the bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Always prompt before exiting
(setq confirm-kill-emacs 'yes-or-no-p)

;; Hippie-expand (kinda like vim mucomplete) with C-<tab>
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
	try-complete-file-name
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill))
(global-set-key (kbd "C-<tab>") 'hippie-expand)

;; When I send mail from emacs, open the default mail client (because I haven't set up sending mail from emacs yet).
(setq send-mail-function 'mailclient-send-it)

;; Automatically tangle our config.org config file when we save it
(defun dkj/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dkj/org-babel-tangle-config)))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
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

(setq org-directory "~/Documents/org/")
(setq org-agenda-files '("~/Documents/org/"))

(setq org-export-backends '(ascii html icalendar latex md odt))

(use-package org-drill)

(use-package org-journal)
(with-eval-after-load 'org-journal 
  (setq org-journal-dir "~/Documents/org/journal/")
  (add-to-list 'org-agenda-files (expand-file-name "~/Documents/org/journal/"))
  (setq org-journal-file-format "%Y-%m-%d.org"))

(use-package which-key)
(require 'which-key)
(which-key-mode)

(use-package magit)

(use-package markdown-mode)
