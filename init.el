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
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Always have the menu bar :)
(menu-bar-mode 1)

;; Show column number in the modeline
(column-number-mode 1)

;; Always prompt before exiting
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; Turn off the beeping with visible-bell
(setq visible-bell t)

;; 4 space tabs
(setq-default tab-width 4)

;; cmd for meta on macos
(when (eq system-type 'darwin)
  (setq
   ns-command-modifier 'meta))

;; unbind set-goal column
;; I'm always accidentally pressing this
(global-unset-key (kbd "C-x C-n"))

;; Automatically balance windows when they are created or destroyed
(setq window-combination-resize t)

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
;; but make it a little thick
(setq-default cursor-type '(bar . 3))

;; Add more context when scrolling around
(setq next-screen-context-lines 10)

;; Move to the top or bottom of the buffer when scrolling
(setq scroll-error-top-bottom 1)

;; Bigger global mark ring
(setq global-mark-ring-max 50)

;; Show column 80
(setq-default fill-column 80)
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
(setq search-whitespace-regexp ".*?")
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

;; Terminal settings
(unless (display-graphic-p)
  ;; Let xterm-compatible terminals copy-paste from emacs
  (setq xterm-extra-capabilities '(setSelection))
  ;; Allow the mouse in terminal mode
  (xterm-mouse-mode 1)
  (defun dkj/xterm-mouse-refresh ()
	(interactive)
	(cond ((equal xterm-mouse-mode t)
		   (progn
			 (xterm-mouse-mode -1)
			 (xterm-mouse-mode 1)))
		  (t (xterm-mouse-mode 1))))
  (add-function :after after-focus-change-function
				#'dkj/xterm-mouse-refresh))

;; No maximum terminal buffer sizes
(setq-default comint-buffer-maximum-size 0)
(setq-default term-buffer-maximum-size 0)
(setq-default eshell-buffer-maximum-lines 0)

;; ediff settings [[https://www.youtube.com/watch?v=pSvsAutseO0][from prot]]
(setq ediff-split-window-function 'split-window-horizontally  ; vert
	  ediff-window-setup-function 'ediff-setup-windows-plain) ; no float

;; Made M-x grep use rg by default
(setq grep-command "rg -n -H --no-heading -e ")

;; List all files and human-readable sizes in dired
(setq-default dired-listing-switches "-lash")

;; Don't show eldoc in the minibuffer
(setq-default eldoc-echo-area-use-multiline-p nil)

;; agenda split
(defun dkj/agenda-split ()
  (split-window-right))

;; run async commands in new buffers when old ones are in use
;; without confirmation
(setq async-shell-command-buffer 'new-buffer)

;; Make shell-command and friends read .bashrc
(setq shell-command-switch "-ic")

(defface my-red-face '((t (:background "#960b0b"))) "Face for RED words")
(defface my-green-face '((t (:background "#214a2c"))) "Face for GREEN words")
(defface my-refactor-face '((t (:background "#630b96"))) "Face for REFACTOR words")

(defun dkj/highlight-words ()
  (interactive)
  (highlight-phrase "RED" 'my-red-face)
  (highlight-phrase "GREEN" 'my-green-face)
  (highlight-phrase "REFACTOR" 'my-refactor-face))

(add-hook 'window-configuration-change-hook 'dkj/highlight-words)

(define-prefix-command 'dkj-keys)
(global-set-key (kbd "C-t") #'dkj-keys)

(defun dkj/tab-next-or-other-frame (prefix)
  "Call tab-next without a prefix or other-frame with"
  (interactive "P")
  (if (equal prefix '(4)) (other-frame 1) (tab-next)))

;; Shorter tab-next and other-frame binding
(define-key dkj-keys (kbd "C-o") #'dkj/tab-next-or-other-frame)

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

;; Binding for grepping
(define-key dkj-keys (kbd "C-/") #'grep)

;; Binding global-subword-mode, which makes word-type operations CamelCase aware
(define-key dkj-keys (kbd "C-s") #'global-subword-mode)

;; Easily duplicate lines or selections
(define-key dkj-keys (kbd "C-y") #'duplicate-dwim)

;; Quick recompile
(define-key dkj-keys (kbd "C-r") #'recompile)

;; Quick export org to gfm
(define-key dkj-keys (kbd "C-e") #'org-gfm-export-as-markdown)

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

;; Do something useful with these that seems roughly congruent with existing C- vs M- movement semantics
(global-set-key (kbd "M-p") #'previous-logical-line)
(global-set-key (kbd "M-n") #'next-logical-line)

(define-key global-map [menu-bar dkj]
			(cons "DKJ" (make-sparse-keymap "DKJ")))

(define-key global-map
			[menu-bar dkj end-macro]
			'("Macro - End/Call" . kmacro-end-or-call-macro))

(define-key global-map
			[menu-bar dkj begin-macro]
			'("Macro - Begin/Counter" . kmacro-start-macro-or-insert-counter))

(define-key global-map
			[menu-bar dkj open-config]
			'("Open config" . dkj/open-config))

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

(defun dkj/polya (prefix)
  "Insert Polya's How to Solve It steps as headers (or list items)"
  (interactive "P")
  (let ((dkj/polya-headers (lambda ()
							 (org-insert-subheading 1)
							 (insert "Problem")
							 (org-insert-heading)
							 (insert "Plan")
							 (org-insert-heading)
							 (insert "Process")
							 (org-insert-heading)
							 (insert "Past")))
		(dkj/polya-items (lambda ()
						   (org-insert-item)
						   (insert "Problem")
						   (org-insert-item)
						   (insert "Plan")
						   (org-insert-item)
						   (insert "Process")
						   (org-insert-item)
						   (insert "Past"))))
	(if (equal prefix '(4))
		(funcall dkj/polya-items)
	  (funcall dkj/polya-headers))))

(setq org-directory "~/org"
	  org-default-notes-file "~/org/inbox.org"
	  org-id-locations-file "~/org/.org-id-locations"
	  org-startup-truncated nil
	  org-ellipsis ">>"
	  org-id-link-to-org-use-id 't
	  org-image-actual-width 600
	  org-edit-src-content-indentation 0
	  org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
	  org-global-properties '(("Effort_ALL" .  "10 30 90 270"))
	  ;; Better default header creation
	  org-M-RET-may-split-line '((default . nil)))

;; work-around  for org-ctags obnoxious behavior
(with-eval-after-load 'org-ctags (setq org-open-link-functions nil))

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

;; Much more complicated fuzzy-completing clock into header from clock history
;; Written by Gemini
(defun dkj/org-clock-anywhere ()
  "Select a task from clock history and clock in.
   Robust, aligned, and face-safe."
  (interactive)
  (require 'org-clock)
  (unless org-clock-history (org-clock-load))
  (let* ((header-width 60)
        (candidates
         (cl-loop for marker in org-clock-history
                  for buf = (marker-buffer marker)
                  when (and buf (buffer-live-p buf))
                  when (with-current-buffer buf
                         (save-excursion
                           (save-restriction
                             (widen)
                             (goto-char marker)
                             (when (org-at-heading-p)
                               (let* ((raw (org-link-display-format (org-get-heading t t t t)))
                                      (head (truncate-string-to-width raw header-width 0 nil "..."))
                                      (key (concat head (make-string (max 0 (- header-width (string-width head))) ?\s)
                                                   (propertize (format "\0:%d" (line-number-at-pos)) 'invisible t))))
                                 (cons (propertize key 
                                                   'dkj-todo (org-get-todo-state)
                                                   'dkj-path (mapconcat #'identity (cons (buffer-name) (org-get-outline-path nil t)) "/"))
                                       marker))))))
                  collect it)))
    (let ((completion-extra-properties
           `(:affixation-function
             (lambda (keys)
               (mapcar (lambda (k)
                         (list k
                               (if-let ((todo (get-text-property 0 'dkj-todo k)))
                                   (format "%-5s " (propertize todo 'face (org-get-todo-face todo))) 
                                 "      ")
                               (propertize (format "   | %s" (get-text-property 0 'dkj-path k)) 
                                           'face 'org-special-keyword)))
                       keys))))
          (vertico-sort-function nil))
      (when-let* ((sel (completing-read "Clock In: " candidates nil t))
                  (marker (cdr (assoc sel candidates))))
        (with-current-buffer (marker-buffer marker)
          (save-restriction
            (widen)
            (goto-char marker)
            (org-clock-in)))))))
(define-key dkj-keys (kbd "C-i") #'dkj/org-clock-anywhere)

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

;; Non duplicating async-shell-command
(defun dkj/async-shell-command (command &optional buffer)
  "Run COMMAND asynchronously, but only if a previous async-shell-command
  is not already running (based on the default *async-shell-output* buffer).
  Optional second argument BUFFER is passed to async-shell-command."
  (interactive "sAsync command: ")
  (let* ((output-buffer (or buffer "*Async Shell Command*"))
         (existing-process (get-buffer-process output-buffer)))
    (if existing-process
        ;; A non-nil value means a process is running
        (message "An async shell command is already running in %s! (Process: %s)" 
                 output-buffer (process-name existing-process))
      ;; No running process found
      (async-shell-command command output-buffer))))

(add-to-list 'display-buffer-alist
			 '("*dkj/org-sync*" display-buffer-no-window (nil)))
;; Quickly sync org files through git
(defun dkj/org-sync ()
  (interactive)
  (org-save-all-org-buffers)
  (let ((default-directory "~/org/"))
    (let* ((status (shell-command-to-string "git status -s"))
           (files (replace-regexp-in-string "\n. ." ", " status))
           (files (replace-regexp-in-string "\n" ", " files))
           (files (replace-regexp-in-string ", $" "" files))
           (message (format "[Emacs] %s" files)))
      (dkj/async-shell-command
	   (format "git add -A && git diff-index --quiet HEAD || git commit -m \"%s\" && git pull && git push" message)
	   "*dkj/org-sync*"))))
(add-function :after after-focus-change-function #'dkj/org-sync)

(require 'org-agenda)

;; define a main view to use in the following functions
(defun dkj/agenda-main-view ()
  (org-agenda nil "n"))

;; define a secondary view to use in the following functions
(defun dkj/agenda-main-one-window ()
  (dkj/agenda-main-view)
  (delete-other-windows))

(defun dkj/present-agenda-and-clocked ()
  "Open the agenda and the currently clocked task side by side."
  (interactive)
  (progn
	(dkj/agenda-main-view)
	(delete-other-windows)
	(dkj/agenda-split)
	(org-agenda-redo-all)
	(other-window 1)
	(org-clock-goto)
	(recenter-top-bottom 0)))

(defun dkj/open-agenda-main-view (prefix)
  "Open the main view of my agenda."
  (interactive "P")
  (dkj/org-sync)
  (progn
	(if (equal major-mode 'org-agenda-mode) (delete-other-windows))
	(setq current-prefix-arg nil)
	(cond
	 ((equal prefix '(4)) (dkj/present-agenda-and-clocked))
	 ((equal prefix '(16)) (dkj/agenda-main-one-window))
	 (t (dkj/agenda-main-view)))))

;; Open agenda through the menu bar
(define-key global-map
			[menu-bar dkj open-agenda-main-view]
			'("Open agenda" . dkj/open-agenda-main-view))

;; Open the main view of the agenda
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
									 (todo "BLCK" ((org-agenda-overriding-header "Blocked tasks")))
									 (todo "PROG" ((org-agenda-overriding-header "In-progress tasks")))
									 (todo "" ((org-agenda-files '("~/org/inbox.org"))
											   (org-agenda-overriding-header "Inbox tasks")))))
								   ("h"
									"Next steps at home organized by sizes"
									((tags-todo "+@home/TODO" ((org-agenda-todo-ignore-deadlines 'all)
															   (org-agenda-todo-ignore-scheduled 'all)))))
								   ("o"
									"Next steps at anywhere organized by sizes"
									((tags-todo "+@out/TODO" ((org-agenda-todo-ignore-deadlines 'all)
															  (org-agenda-todo-ignore-scheduled 'all)))))))

;; Agenda sorting order
(setq org-agenda-sorting-strategy '((agenda time-up todo-state-down deadline-down category-keep effort-down)
									(todo todo-state-down category-keep effort-down)
									(tags todo-state-down effort-down)
									(search category-keep effort-down)))

;; Agenda clockreport settings
(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 6 :tags t))

(defun dkj/format-n-breadcrumbs (n)
  "Formats the first and last n-1 headers for an org item for my agenda."
  (let* ((breadcrumbs (when (derived-mode-p 'org-mode)
						(org-get-outline-path)))
		 (blength (length breadcrumbs))
		 (extra (if (> blength n) '(".") '()))
		 (first (cons (car breadcrumbs)
					  extra))
		 (n1 (max (- (min blength n) 1) 0))
		 (last-n (seq-subseq breadcrumbs
							 (- blength n1)
							 blength)))
	(format "%-25.25s" (string-join (append first last-n) ">"))))

;; Number of breadcrumbs to format into my agenda prefix
(setq breadcrumbs-to-format 2)
;; Set prefix to use top level header instead of file name in todo list
(setq org-agenda-prefix-format
	  '((agenda . "%(dkj/format-n-breadcrumbs breadcrumbs-to-format) %?-12t %?|e % s")
		(todo . "%(dkj/format-n-breadcrumbs breadcrumbs-to-format) %s %?|e ")
		(tags . "%(dkj/format-n-breadcrumbs breadcrumbs-to-format) %s %?|e ")
		(search . "%-12:c %?|e ")))

;; Remap h (org-agenda-holidays) to org-revert-all-org-buffers
(with-eval-after-load "org"
  (define-key org-agenda-mode-map (kbd "h") #'org-revert-all-org-buffers))

(setq org-todo-keywords
	  '((sequence "TODO(t)" "PROG(p)" "BLCK(b)" "|" "DONE(d!)" "CNCL(c!)"))
	  org-clock-into-drawer t
	  org-log-into-drawer t)

;; Switch to "PROG" when clocked in, if the header is a TODO
(defun dkj/prog-when-clock-if-todo (state)
  (message state)
  (cond ((equal state nil) state)
		(t "PROG")))
(setq org-clock-in-switch-to-state #'dkj/prog-when-clock-if-todo)

(setq org-tag-persistent-alist '(;; Contexts
								 ("@home" . ?h)
								 ("@out" . ?o)))

(setq org-capture-templates
	  (quote (("t" "Todo" entry (file "~/org/inbox.org")
			   "* TODO %?\n%U\n%a\n")
			  ("m" "Meeting" entry (file+olp+datetree "~/org/meetings.org")
			   "* %? :MEETING:\n%U\n")
			  ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
			   "* %? :JOURNAL:\n%U\n" :clock-in t :clock-keep t))))

;; Show lots of clocking history so it's easy to pick items off the C-t C-i list
(setq org-clock-history-length 40)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
(org-clock-persistence-insinuate)
;; Autosave clock metadata
(run-at-time nil 300 'org-clock-save)
;; Set clock duration format to never aggregate up to days
(setq org-duration-format (quote h:mm))

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
	  org-export-headline-levels 10
	  org-export-use-babel nil)

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
								  "projects.org"
								  "consume.org"))

(defun dkj/org-ical-export ()
  (interactive)
  (setq current-agenda-files org-agenda-files)
  (setq org-agenda-files dkj/org-ical-agenda-files)
  (org-icalendar-combine-agenda-files)
  (setq org-agenda-files current-agenda-files))

(setq org-export-backends '(ascii html icalendar latex md odt))

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
(setq use-package-compute-statistics t)

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
(setq dkj/theme-dark 'modus-vivendi-tinted)

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

;; Default to dark theme
(load-theme dkj/theme-dark t)

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

(use-package go-mode)

;; execute Go in org source blocks
(use-package ob-go)

(use-package elixir-mode)

(use-package ob-go)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (plantuml . t)
   (go . t)
   (shell . t)))

(setq org-babel-python-command "python3")

(setq org-plantuml-exec-mode 'plantuml)

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
  (("C-;" . #'avy-goto-char-2))
  (:map org-mode-map
		("C-;" . #'avy-goto-char-2))
  (:map isearch-mode-map
		("C-;" . #'avy-isearch))
  :config
  (setq avy-timeout-seconds 0.25
		avy-single-candidate-jump nil)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package ace-window
  :ensure t
  :bind
  (("C-M-;" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
		aw-dispatch-always t))

(use-package vundo)

(with-eval-after-load "tetris-mode"
  (define-key tetris-mode-map (kbd "z") #'tetris-rotate-next)
  (define-key tetris-mode-map (kbd "x") #'tetris-rotate-prev)
  (define-key tetris-mode-map (kbd "<up>") #'tetris-move-bottom))

(use-package org-noter
  :config
  (setq org-noter-highlight-selected-text t
		org-noter-max-short-selected-text-length 0
		org-noter-swap-window t))

(defun dkj/mobile-org-noter ()
  "Call org-noter in a way that sets everything up perfectly for mobile device usage."
  (interactive)
  (let* ((org-noter-notes-window-location 'vertical-split)
		 (org-noter-doc-split-fraction '(0.1 . 0.9)))
	(org-noter)))

(define-key global-map [menu-bar mobile-reading]
			(cons "READ" (make-sparse-keymap "READ")))

(defun dkj/goto-id-mobile-org-noter (id)
  (org-id-open id t)
  (dkj/mobile-org-noter))

(define-key global-map
			[menu-bar mobile-reading noter2]
			'("BrilliantFriend" . (lambda () (interactive)
									(dkj/goto-id-mobile-org-noter
									 "7a7229e7-1dde-4e8c-8e6b-8cd9915f6eb1"))))
(define-key global-map
			[menu-bar mobile-reading noter1]
			'("ElixirInAction" . (lambda () (interactive)
								   (dkj/goto-id-mobile-org-noter
									"0467cb4c-2045-4339-8097-855d7caf09fd"))))
(define-key global-map
			[menu-bar mobile-reading book-club]
			'("Fanon" . (lambda () (interactive)
						  (dkj/goto-id-mobile-org-noter
						   "672b1b9a-6572-4eb6-aeb0-92dc8d46c2fa"))))

(define-key global-map
			[separator-4] menu-bar-separator) 

(define-key global-map
			[menu-bar mobile-reading noter-kill]
			'("Kill noter" . org-noter-kill-session))

(defun dkj/noter-insert-note-and-save-all ()
  (interactive)
  (org-noter-insert-precise-note)
  (save-some-buffers t))

(define-key global-map
			[menu-bar mobile-reading org-noter-insert-precise-note]
			'("Insert note" . dkj/noter-insert-note-and-save-all))

(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-save-place-file "~/org/nov-places"
		nov-text-width 50))

(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t))

(defun dkj/nov-display-setup ()
  (when (require 'visual-fill-column nil t)
	(setq-local visual-fill-column-width
				(1+ nov-text-width))
    (visual-line-fill-column-mode 1)))

(add-hook 'nov-mode-hook 'dkj/nov-display-setup)

(use-package speed-type)

(use-package eat)

(use-package ox-gfm)
(eval-after-load "org"
  '(require 'ox-gfm nil t))

(use-package gptel
  :config
  (setq gptel-model 'gemini-2.0-flash)
  (setq gptel-backend (gptel-make-gemini "Gemini"
                        :key (getenv "GEMINI_API_KEY")
                        :stream t))
  :bind
  ("C-`" . gptel-send))

(use-package fsrs
  :vc (:url "https://github.com/bohonghuang/lisp-fsrs"
			:rev :newest)
  :defer t)

(use-package org-srs
  :vc (:url "https://github.com/bohonghuang/org-srs"
			:rev :newest)
  :defer t
  :hook (org-mode . org-srs-embed-overlay-mode)
  :bind (:map org-mode-map
			  ("<f5>" . org-srs-review-rate-easy)
			  ("<f6>" . org-srs-review-rate-good)
			  ("<f7>" . org-srs-review-rate-hard)
			  ("<f8>" . org-srs-review-rate-again)))

(use-package keyfreq)
(keyfreq-mode t)
(keyfreq-autosave-mode t)

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode org-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(when (eq system-type 'android)
  ;; tool bar is cool and should be on bottom
  (tool-bar-mode 1)
  (set-frame-parameter nil 'tool-bar-position 'bottom)
  (set-frame-parameter nil 'tool-bar-lines 1)
  ;; bigger font
  (dkj/font-height 160)
  ;; always display keyboard
  (setq touch-screen-display-keyboard t)
  ;; extra light and dark themes for eink
  (setq dkj/theme-light 'modus-operandi)
  (setq dkj/theme-dark 'modus-vivendi)
  (load-theme dkj/theme-light t)
  ;; smooth scrolling
  (setq touch-screen-precision-scroll t)
  ;; dont blink cursor (particularly for eink)
  (setq-default blink-cursor-mode nil)
  ;; split below for smaller screen
  (defun dkj/agenda-split ()
	(split-window-below))
  ;; special bindings
  (global-set-key (kbd "<volume-down>") #'scroll-up-command)
  (global-set-key (kbd "<volume-up>") #'scroll-down-command)
  (define-key doc-view-mode-map (kbd "<volume-down>") #'doc-view-scroll-up-or-next-page)
  (define-key doc-view-mode-map (kbd "<volume-up>") #'doc-view-scroll-down-or-previous-page)
  (define-key nov-mode-map (kbd "<volume-down>") #'nov-scroll-up)
  (define-key nov-mode-map (kbd "<volume-up>") #'nov-scroll-down)
  (global-set-key (kbd "A-e") #'avy-goto-char-2)
  (global-set-key (kbd "A-d") #'delete-other-windows))

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
