;;; -*- lexical-binding: t -*-

(defun dkj/org-babel-tangle-config ()
  "Automatically tangle our config.org config file when we save it"
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dkj/org-babel-tangle-config)))

;; Turn off the tool bar and scroll bar
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
;; Always have the menu bar :)
(menu-bar-mode 1)

;; Show column number in the modeline
(column-number-mode 1)

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

;; Winner mode remembers my window layouts
(winner-mode 1)

;; Save minibuffer history across sessions
(savehist-mode 1)

;; Automatically pull changes to files from disk
(global-auto-revert-mode 1)

;; Default to lax whitespace in isearch and match any char
(setq search-whitespace-regexp ".*")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)

;; Override disabled commands
(put 'narrow-to-region 'disabled nil)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Make bash the default explicit shell
(setq explicit-shell-file-name "/bin/bash")

;; Use bash_history in shell mode
(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(defun my-shell-mode-hook ()
  (setq comint-input-ring-file-name "~/.bash_history")
  (comint-read-input-ring t))

;; Don't use a special shell history file in tramp
(setq tramp-histfile-override nil)

;; Increase shell mode history ring
(setq comint-input-ring-size 100000)

;; Let xterm-compatible terminals copy-paste from emacs
(setq xterm-extra-capabilities '(setSelection))

;; ediff settings [[https://www.youtube.com/watch?v=pSvsAutseO0][from prot]]
(setq ediff-split-window-function 'split-window-horizontally  ; vert
      ediff-window-setup-function 'ediff-setup-windows-plain) ; no float

;; Android bindings
(global-set-key (kbd "<volume-down>") #'execute-extended-command)
(global-set-key (kbd "<volume-up>") #'winner-undo)

(define-prefix-command 'dkj-keys)
(global-set-key (kbd "C-t") #'dkj-keys)

;; Shorter tab-next binding
(define-key dkj-keys (kbd "C-o") #'tab-next)

;; Easily store links to org headers
(define-key dkj-keys (kbd "C-l") #'org-store-link)
;; Capture something
(define-key dkj-keys (kbd "C-t") #'org-capture)

;; Open the agenda
(define-key dkj-keys (kbd "C-a") #'org-agenda)

;; Nicer winner-mode bindings
(define-key dkj-keys (kbd "C-p") #'winner-undo)
(define-key dkj-keys (kbd "C-n") #'winner-redo)

;; Better macro bindings
(define-key dkj-keys (kbd "C-9") #'kmacro-start-macro-or-insert-counter)
(define-key dkj-keys (kbd "C-0") #'kmacro-end-or-call-macro)

;; Reserve this for tmux. Previously toggle-input-method
(global-unset-key (kbd "C-\\"))

;; Easier window movement
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-o") #'other-window)

;; Use dwim versions of upcase and downcase instead of char/word/region-specific verions
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)

;; Use cycle-spacing since it replaces just-one-space,
;; delete-horizontal-space, and delete-blank-lines all in one
(global-set-key (kbd "M-SPC") #'cycle-spacing)

;; Bind M-/ to dabbrev-completion instead of dabbrev-expand to use capf
(global-set-key (kbd "M-/") #'dabbrev-completion)

(define-key global-map [menu-bar dkj]
	    (cons "DKJ" (make-sparse-keymap "DKJ")))

(define-key global-map
	    [menu-bar dkj end-macro]
	    '("Macro - End/Call" . kmacro-end-or-call-macro))

(define-key global-map
	    [menu-bar dkj begin-macro]
	    '("Macro - Begin/Counter" . kmacro-start-macro-or-insert-counter))

;; C-t C-h to open this file, my config
(defun dkj/open-config ()
  "Open this file."
  (interactive)
  (push-mark)
  (find-file "~/.emacs.d/README.org"))
(define-key dkj-keys (kbd "C-h") #'dkj/open-config)

;; Pulse for a little longer than the default
(setq pulse-delay 0.1)
;; Pulse the line when I get lost
(defun dkj/pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (command '(scroll-up-command scroll-down-command
				     recenter-top-bottom other-window))
  (advice-add command :after #'dkj/pulse-line))

(setq org-directory "~/org"
      org-default-notes-file "~/org/inbox.org"
      org-id-locations-file "~/org/.org-id-locations"
      org-startup-truncated nil
      org-ellipsis ">>"
      org-id-link-to-org-use-id 'create-if-interactive
      org-image-actual-width 600)

;; Make inserting new list items a little cleaner
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "M-<return>") #'org-insert-item)
  (define-key org-mode-map (kbd "C-<return>") #'org-insert-heading))

;; Create a link to an org header interactively
;; using the same backend as refile
;; taken from https://www.reddit.com/r/emacs/comments/qblthi/how_to_link_to_headings_in_another_org_file_with/
(defun dkj/org-id-insert-link ()
  "Insert at point a link to any heading from 'org-agenda-files'."
  (interactive)
  (let ((buffer-pos
	 (org-id-find
	  (org-id-get-with-outline-path-completion '((nil :maxlevel . 100)
						     (org-agenda-files :maxlevel . 5))))))
    (save-excursion
      (with-current-buffer (get-file-buffer (car buffer-pos))
	(goto-char (cdr buffer-pos))
	(call-interactively 'org-store-link)))
    (org-insert-all-links 1 "" " ")))
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c l") #'dkj/org-id-insert-link))

;; "One" button org-add-note to clocked workflow
(defun dkj/create-org-store-log-note-and-save (m)
  (defun dkj/org-store-log-note-and-save () ; This only works with lexical binding
    (org-store-log-note)
    (save-some-buffers t
		       (lambda ()
			 (eq (marker-buffer m) (current-buffer))))))

(defun dkj/org-add-note-clocked ()
  (interactive)
  ;; Marker logic copied from org-clock-goto
  (let* ((recent nil)
	 (m (cond
	     ((org-clocking-p) org-clock-marker)
	     ((and org-clock-goto-may-find-recent-task
		   (car org-clock-history)
		   (marker-buffer (car org-clock-history)))
	      (setq recent t)
	      (car org-clock-history))
	     (t (user-error "No active or recent clock task")))))
    (if recent ;; this is also from org-clock-goto
	(message "No running clock, this is the most recently clocked task"))
    ;; Copy and merge org-add-log-setup and org-add-log-note
    ;; but using clocked marker, keeping the current window
    ;; instead of moving to the target org heading
    ;; and not doing extra stuff that's not relevant to this case
    (move-marker org-log-note-marker (marker-position m) (marker-buffer m))
    (setq org-log-note-purpose 'note
	  org-log-note-effective-time (org-current-effective-time)
	  org-log-note-this-command this-command
	  org-log-note-recursion-depth (recursion-depth)
	  org-log-post-message nil) ;; prevents storing the log from sending an extra "Entry repeats" message
    (when (and (equal org-log-note-this-command this-command)
	       (= org-log-note-recursion-depth (recursion-depth)))
      (setq org-log-note-window-configuration (current-window-configuration))
      (delete-other-windows)
      (move-marker org-log-note-return-to (point))
      (org-switch-to-buffer-other-window "*Org Note*")
      (erase-buffer)
      (let ((org-inhibit-startup t)) (org-mode))
      (insert "# Insert note for this entry.\n# Finish with C-c C-c, or cancel with C-c C-k.\n\n")
      (when org-log-note-extra (insert org-log-note-extra))
      (setq-local org-finish-function (dkj/create-org-store-log-note-and-save m))
      (run-hooks 'org-log-buffer-setup-hook))))
(global-set-key (kbd "C-z") #'dkj/org-add-note-clocked)

(require 'org-agenda)

;; define a main view to use in the following functions
(defun dkj/agenda-main-view ()
  (org-agenda nil "n"))

;; define a secondary view to use in the following functions
(defun dkj/agenda-alt-view ()
  (org-agenda nil "N"))

(defun dkj/present-agenda-and-clocked ()
  "Open the agenda and the currently clocked task side by side."
  (interactive)
  (progn
    (dkj/agenda-main-view)
    (delete-other-windows)
    (split-window-right)
    (org-agenda-redo-all)
    (other-window 1)
    (org-clock-goto)
    (recenter-top-bottom 0)))

(defun dkj/open-agenda-main-view (prefix)
  "Open the main view of my agenda."
  (interactive "P")
  (progn
    (if (equal major-mode 'org-agenda-mode) (delete-other-windows))
    (setq current-prefix-arg nil)
    (cond
     ((equal prefix '(4)) (dkj/present-agenda-and-clocked))
     ((equal prefix '(16)) (dkj/agenda-alt-view))
     (t (dkj/agenda-main-view)))))

;; Open agenda through the menu bar
(define-key global-map
	    [menu-bar dkj open-agenda-main-view]
	    '("Open agenda" . dkj/open-agenda-main-view))

;; Open the main view of the agenda with f12
(global-set-key (kbd "C-o") #'dkj/open-agenda-main-view)

;; ~/org for agenda and refile settings
(setq org-agenda-files '("~/org")
      org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path 'file
      org-agenda-span 'day
      org-agenda-tags-todo-honor-ignore-options t)

;; Open my custom agenda view
(setq org-agenda-custom-commands '(("n"
				    "Today's agenda"
				    ((agenda "" ((org-deadline-warning-days 7)))
				     (todo "" ((org-agenda-files '("~/org/inbox.org"))))))
				   ("N"
				    "Todos in Do, Decide, Delegate, Delete order"
				    ((tags-todo "+important+urgent" ((org-agenda-todo-ignore-deadlines 'all)
								     (org-agenda-todo-ignore-scheduled 'all)))
				     (tags-todo "+important-urgent" ((org-agenda-todo-ignore-deadlines 'all)
								     (org-agenda-todo-ignore-scheduled 'all)))
				     (tags-todo "-important+urgent" ((org-agenda-todo-ignore-deadlines 'all)
								     (org-agenda-todo-ignore-scheduled 'all)))
				     (tags-todo "-important-urgent" ((org-agenda-todo-ignore-deadlines 'all)
								     (org-agenda-todo-ignore-scheduled 'all)))))))

;; Agenda sorting order
(setq org-agenda-sorting-strategy '((agenda time-up todo-state-down category-keep)
				    (todo todo-state-down category-keep)
				    (tags todo-state-down)
				    (search category-keep)))

;; Agenda clockreport settings
(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 6 :tags t))

(defun dkj/format-n-breadcrumbs (n)
  "Formats the top n headers for an org item for my agenda."
  (let* ((breadcrumbs (org-get-outline-path))
	 (first-n (seq-subseq breadcrumbs
			      0
			      (min n
				   (length breadcrumbs)))))
    (format "%-25.25s" (if first-n
			   (string-join first-n ">")
			 ""))))

;; Number of breadcrumbs to format into my agenda prefix
(setq breadcrumbs-to-format 2)
;; Set prefix to use top level header instead of file name in todo list
(setq org-agenda-prefix-format
      '((agenda . "%(dkj/format-n-breadcrumbs breadcrumbs-to-format) %?-12t% s")
	(todo . "%(dkj/format-n-breadcrumbs breadcrumbs-to-format) %s")
	(tags . "%(dkj/format-n-breadcrumbs breadcrumbs-to-format) %s")
	(search . "%-12:c")))

;; Remap h (org-agenda-holidays) to org-revert-all-org-buffers
(with-eval-after-load "org"
  (define-key org-agenda-mode-map (kbd "h") #'org-revert-all-org-buffers))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROG(p)" "|" "DONE(d!)" "CNCL(c!)"))
      org-clock-into-drawer t
      org-log-into-drawer t)

;; Switch to "PROG" when clocked in, unless we're just clocking in a capture buffer
(defun dkj/prog-when-clock-if-not-cap (state)
  (cond ((and (boundp 'org-capture-mode) org-capture-mode) state)
	(t "PROG")))
(setq org-clock-in-switch-to-state #'dkj/prog-when-clock-if-not-cap)

(setq org-tag-persistent-alist '(("important" . ?i)
				 ("urgent"    . ?u)))

(setq org-capture-templates
      (quote (("t" "Todo" entry (file "~/org/inbox.org")
	       "* TODO %?\n%U\n%a\n" :clock-in t :clock-keep t)
	      ("m" "Meeting" entry (file+olp+datetree "~/org/meetings.org")
	       "* %? :MEETING:\n%U\n" :clock-in t :clock-keep t)
	      ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
	       "* %? :JOURNAL:\n%U\n" :clock-in t :clock-keep t))))

;; Show lot of clocking history so it's easy to pick items off the C-t C-i list
(setq org-clock-history-length 25)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Set clock duration format to never aggregate up to days
(setq org-duration-format (quote h:mm))
;; Show current clock period instead of defaulting to total clocked time
(setq org-clock-mode-line-total 'current)

;; Define things that show up as issues in clock check (v c in org-agenda)
;; Only thing I've changed is lowering the default max-gap from 5 minutes to 1
;; and lowering the default max-duration from 10 hours to 5 hours.
(setq org-agenda-clock-consistency-checks '(:max-duration "5:00"
							  :min-duration 0
							  :max-gap "0:01"
							  :gap-ok-around
							  ("4:00")
							  :default-face
							  ((:background "DarkRed")
							   (:foreground "white"))
							  :overlap-face nil
							  :gap-face nil
							  :no-end-time-face nil
							  :long-face nil
							  :short-face nil))

(defun dkj/global-clock-in ()
  (interactive)
  (org-clock-in '(4)))
(define-key dkj-keys (kbd "C-i") #'dkj/global-clock-in)

  ;;;;; LOG BASED WORKFLOW BINDINGS I WANT TO KEEP HERE FOR NOW ;;;;;
;; (defun dkj/log-at-marker (marker)
;;   (pop-to-buffer-same-window (marker-buffer marker))
;;   (goto-char marker)
;;   (org-insert-heading '(4))
;;   (when (org-clocking-p) (org-clock-out))
;;   (org-clock-in))

;; (defun dkj/get-log-end-marker ()
;;   (let ((logb (get-buffer "log.org")))
;;     (set-marker (make-marker) (+ 1 (buffer-size logb)) logb)))

;; (defun dkj/smart-log ()
;;   (let ((jump-marker (cond
;; 		      ;; If in the log, log at point
;; 		      ((string= (buffer-name (window-buffer (minibuffer-selected-window)))
;; 				"log.org")
;; 		       (point-marker))
;; 		      ;; If clocked in log, log at clocked
;; 		      ((and (org-clocking-p)
;; 			    (string= (buffer-name (marker-buffer org-clock-marker))
;; 				     "log.org"))
;; 		       org-clock-marker)
;; 		      ;; Else log at end
;; 		      (t
;; 		       (dkj/get-log-end-marker)))))
;;     (dkj/log-at-marker jump-marker)))

;; (defun dkj/log-at-end ()
;;   (dkj/log-at-marker (dkj/get-log-end-marker)))

;; (defun dkj/new-log ()
;;   (interactive)
;;   (cond
;;    ((equal current-prefix-arg nil) (dkj/smart-log))
;;    ((equal current-prefix-arg '(4)) (dkj/log-at-end))))

;; (define-key dkj-keys (kbd "C-<return>") #'dkj/new-log)

(setq org-export-with-sub-superscripts nil
      org-export-with-section-numbers nil
      org-export-with-toc nil
      org-export-headline-levels 10)

(setq org-icalendar-store-UID 't
      org-icalendar-use-deadline '(event-if-todo-not-done event-if-not-todo)
      org-icalendar-use-scheduled '(event-if-todo-not-done event-if-not-todo)
      org-icalendar-scheduled-summary-prefix "S: "
      org-icalendar-deadline-summary-prefix "DL: "
      org-icalendar-combined-name "David Org Export"
      org-agenda-default-appointment-duration 30
      dkj/org-ical-agenda-files '("inbox.org"
				  "init.org"
				  "journal.org"
				  "meetings.org"
				  "projects.org"))

(defun dkj/org-ical-export ()
  (interactive)
  (setq current-agenda-files org-agenda-files)
  (setq org-agenda-files dkj/org-ical-agenda-files)
  (org-icalendar-combine-agenda-files)
  (setq org-agenda-files current-agenda-files))

(setq org-export-backends '(ascii html icalendar latex md odt))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq org-babel-python-command "python3")

(defun dkj/extract-code-block-noweb (name)
  "Extracts a block of code from an org code block,
surrounding it with a new named code block,
and leaving a noweb reference in its place."
  (interactive "MName: " name)
  (let* ((rb (region-beginning))
	 (re (region-end))
	 (region (buffer-substring-no-properties rb re))
	 (lang (car (ignore-errors (org-babel-get-src-block-info))))
	 (noweb-ref (format "<<%s>>" name))
	 (newblock (format "#+name: %s\n#+begin_src %s :noweb yes\n%s\n#+end_src"
			   name
			   lang
			   region)))
    (delete-region rb re)
    (kill-new newblock)
    (insert noweb-ref)
    (indent-region rb re)))
(define-key dkj-keys (kbd "C-k") #'dkj/extract-code-block-noweb)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
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

(use-package which-key
  :config
  (which-key-mode))

;; Themes that I like to have available
(use-package gruvbox-theme)
(use-package material-theme)
(use-package modus-themes) ;; built in now, but to get the tinted themes we need the package, I think

;; Some modus theme customization
(setq modus-themes-org-blocks 'gray-background)

;; Light and dark themes I'm using currently
(setq dkj/theme-light 'modus-operandi-tinted)
(setq dkj/theme-dark 'modus-vivendi)

;; Function to swap between light and dark theme
(defun dkj/swap-themes ()
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (cond
		 ((eq current-theme dkj/theme-light) dkj/theme-dark)
		 ((eq current-theme dkj/theme-dark) dkj/theme-light))
		t)))

;; Bind swapping between light and dark theme to "C-t C-\"
(define-key dkj-keys (kbd "C-\\") #'dkj/swap-themes)

;; Default to dark theme except on Android
;; where I want to default to light theme and get even lighter
;; for the Boox
(cond ((eq system-type 'android)
       (setq dkj/theme-light 'modus-operandi)
       (load-theme dkj/theme-light t))
      (t
       (load-theme dkj/theme-dark t)))

(use-package dot-mode
  :config
  (dot-mode 1)
  (global-dot-mode 1))

;; Remap the default dot-mode bindings to not conflict with my Embark bindings
(with-eval-after-load "dot-mode"
  (define-key dot-mode-map (kbd "C-.") nil)
  (define-key dot-mode-map (kbd "C-M-.") nil)
  (define-key dot-mode-map (kbd "C-c .") nil)
  (define-key dot-mode-map (kbd "C-x C-.") #'dot-mode-execute)
  (define-key dot-mode-map (kbd "C-x C-M-.") #'dot-mode-override))

(use-package magit)

(use-package marginalia
  :ensure t
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless initials basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 1)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  ;; enable the mouse
  (vertico-mouse-mode 1))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for 'corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-scroll-margin 1)        ;; Use scroll margin
  :init
  (global-corfu-mode))

;; Enable indentation+completion using the TAB key.
(setq tab-always-indent 'complete)

(use-package corfu-terminal
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-," . embark-export)      ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  (:map org-mode-map
	("C-," . embark-export))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Use the minimal indicator instead of the default mixed indicator
  (setq embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(setq embark-quit-after-action nil)

(use-package markdown-mode)

(use-package racket-mode)

(use-package gdscript-mode)

(use-package anki-editor)

;; Create a named command for inserting a hiragana from the clipbard
(defalias 'dkj/anki-insert-hiragana-from-clipboard
  (kmacro "M-x a n k i - e d i t o r - i n d e r <backspace> <backspace> <backspace> s e r t - n o t e <return> b a s i c SPC a n d SPC r e <return> C-y <return> M-S-<left> C-c C-n C-e <return> C-y C-n C-e <return>"))

;; Command for creating a new card using a structure in the "a" register
(defalias 'dkj/insert-from-a
  (kmacro "C-u C-<return> C-a C-k C-x r i a C-c C-u C-e" 4 "%d"))

;; Back up and autosave into directories, instead of all over the place
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))

;; Back up by copying instead of moving
(setq backup-by-copying t)

;; Nobody uses double spaces at the end of sentences anymore
(setq sentence-end-double-space nil)

;; Guess indent style from the surrounding file and directory
(unless (package-installed-p 'dtrt-indent) (package-install 'dtrt-indent))
(setq dtrt-indent-global-mode t)

;; Show trailing whitespace
(setq show-trailing-whitespace t)

(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

;; From https://karthinks.com/software/avy-can-do-anything/
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
	(goto-char pt)
	(embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)


(use-package avy
  :ensure t
  :bind
  (("C-;" . avy-goto-char-timer))
  (:map org-mode-map
	("C-;" . avy-goto-char-timer))
  (:map isearch-mode-map
	("C-;" . avy-isearch))
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  (setq avy-timeout-seconds 0.25))

(use-package vundo)

(with-eval-after-load "tetris-mode"
  (define-key tetris-mode-map (kbd "z") #'tetris-rotate-next)
  (define-key tetris-mode-map (kbd "x") #'tetris-rotate-prev)
  (define-key tetris-mode-map (kbd "<up>") #'tetris-move-bottom))

(use-package org-noter
  :config
  (setq org-noter-highlight-selected-text t))

;; (use-package pdf-tools
;;   :ensure t
;;   :init
;;   (pdf-tools-install))

(use-package nov
  :ensure t
  :config
  (setq nov-text-width 80)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Load customize stuff
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load Google stuff if it exists
(setq googel (concat user-emacs-directory "google.el"))
(when (file-exists-p googel)
  (load googel))

;; Load non-Google stuff if it exists
(setq noogel (concat user-emacs-directory "noogle.el"))
(when (file-exists-p noogel)
  (load noogel))
