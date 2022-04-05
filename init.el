;; command is a lot more ergonomic than option
;; also, less confusing when going back and forth between mac and windows
(setq mac-command-modifier 'meta)

;; C-c h to open this file, my config
(defun dkj/open-config ()
  "Open this file"
  (interactive)
  (push-mark)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c h") 'dkj/open-config)

;; Not having equivalents to Vim o/O is killing me
(defun dkj/open-next-line ()
  "Equivalent of Vim's o"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "C-o") 'dkj/open-next-line)

(defun dkj/open-previous-line ()
  "Equivalent of Vim's O"
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "C-S-o") 'dkj/open-previous-line)

;; We want to follow new windows we create
(defun dkj/split-window-below-follow ()
  "Split the window horizontally and move into the new window"
  (interactive)
  (split-window-below)
  (windmove-down))
(global-set-key (kbd "C-x 2") 'dkj/split-window-below-follow)

(defun dkj/split-window-right-follow ()
  "Split the window vertically and move into the new window"
  (interactive)
  (split-window-right)
  (windmove-right))
(global-set-key (kbd "C-x 3") 'dkj/split-window-right-follow)

(defun dkj/kill-line-backwards ()
  "Kill the line backwards. Like the opposite of C-k"
  (interactive)
  (kill-line 0))
(global-set-key (kbd "M-DEL") 'dkj/kill-line-backwards)

;; I can't see a god damn at this small font size
(set-face-attribute 'default nil :height 160)

;; Automatically support view-mode when we're in readonly-mode. From here: https://karthinks.com/software/batteries-included-with-emacs/#view-mode--m-x-view-mode
(setq view-read-only t)

;; Turn off the beeping with visible-bell
(setq visible-bell t)

;; Turn off all the bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Always prompt before exiting
(setq confirm-kill-emacs 'yes-or-no-p)

;; Automatically move to help windows when they're opened
(setq help-window-select t)

;; isearch with regexp by default. Swap bindings with plain isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; When I send mail from emacs, open the default mail client (because I haven't set up sending mail from emacs yet).
(setq send-mail-function 'mailclient-send-it)

(defun dkj/org-babel-tangle-config ()
  "Automatically tangle our config.org config file when we save it"
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

(use-package modus-themes) ;; package is needed before Emacs 28
(require 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-vivendi)

(setq org-directory "~/Documents/org/")
(setq org-agenda-files '("~/Documents/org/"))
(setq org-id-locations-file "~/Documents/org/.org-id-locations")
(setq org-startup-truncated nil)

(setq org-export-backends '(ascii html icalendar latex md odt))

(use-package org-drill)

(use-package org-roam)
(setq org-roam-directory (file-truename "~/Documents/org"))
(setq org-roam-db-location (file-truename "~/Documents/org/org-roam.db"))
(org-roam-db-autosync-mode)

(setq org-roam-dailies-directory "journal/")
(global-set-key (kbd "C-c j") 'org-roam-dailies-goto-today)
(global-set-key (kbd "C-c n") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c b") 'org-roam-dailies-goto-previous-note)
(global-set-key (kbd "C-c f") 'org-roam-dailies-goto-next-note)
(defun dkj/current-time-string ()
  (format-time-string "%H:%M"))
(setq org-roam-dailies-capture-templates '(
					   ("d" "default" entry "* %(dkj/current-time-string) %?" :target
					    (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   ))

(setq org-babel-python-command "python3")

(use-package which-key)
(require 'which-key)
(which-key-mode)

(use-package magit)

(use-package markdown-mode)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))

;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)))

(setq tab-always-indent 'complete)
