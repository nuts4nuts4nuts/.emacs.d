;;; pebble-agenda.el --- Manage Org agenda from a Pebble watch -*- lexical-binding: t; -*-
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (org "9.5"))
;; Keywords: outlines, calendar, hardware
;; URL: https://developer.repebble.com/
;;; Commentary:
;; Enable `pebble-agenda-mode' explicitly to serve configured local Org files.
;; Requiring this library does not open files, generate tokens, or start a server.
;;; Code:
(require 'org)
(require 'org-agenda)
(require 'calendar)
(require 'pebble)
(defvar org-state) ; Dynamically supplied by Org's TODO state-change hooks.

(defgroup pebble-agenda nil "Manage Org from a Pebble." :group 'org :group 'pebble)
(defcustom pebble-agenda-files nil
  "Org files to expose.  Nil uses `org-agenda-files' when mode is enabled."
  :type '(repeat file))
(defcustom pebble-agenda-default-view 'today "Initial watch view."
  :type '(choice (const today) (const upcoming) (const open) (const done)))
(defcustom pebble-agenda-page-size 12 "Tasks per watch page, between 1 and 40."
  :type 'integer)
(defcustom pebble-agenda-refresh-seconds 10 "Refresh interval while enabled."
  :type 'number)
(defcustom pebble-agenda-save-changes t
  "Save each watch edit.  Refuse pre-existing unsaved edits when non-nil."
  :type 'boolean)
(defcustom pebble-agenda-token-file (locate-user-emacs-file "pebble-agenda-token")
  "Private file holding the generated pairing token."
  :type 'file)
(defcustom pebble-agenda-public-url "http://127.0.0.1:8765"
  "Address reachable from the phone.
Configure the listener separately with `pebble-http-host'."
  :type 'string)
(defvar pebble-agenda--entries nil)
(defvar pebble-agenda--actions nil)
(defvar pebble-agenda--view nil)
(defvar pebble-agenda--page 0)
(defvar pebble-agenda--detail nil)
(defvar pebble-agenda--screen 'list)
(defvar pebble-agenda--timer nil)
(defvar pebble-agenda--signature nil)
(defvar pebble-agenda--old-handlers nil)
(defvar pebble-agenda--old-token nil)
(defvar pebble-agenda--owns-server nil)
(defvar pebble-agenda-mode nil)

(defun pebble-agenda--files ()
  (delete-dups
   (mapcar (lambda (file)
             (when (or (file-remote-p file) (string-suffix-p ".gpg" file))
               (user-error "Use local unencrypted Org files for the watch"))
             (unless (file-regular-p file) (user-error "Agenda file is missing: %s" file))
             (file-truename file))
           (or pebble-agenda-files (org-agenda-files t)))))

(defun pebble-agenda--date (timestamp)
  (when timestamp (org-time-string-to-absolute timestamp)))
(defun pebble-agenda--today () (org-today))

(defun pebble-agenda--collect ()
  "Collect local entries without modifying or saving them."
  (let (entries)
    (dolist (file (pebble-agenda--files))
      (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file t))
        (unless (derived-mode-p 'org-mode) (user-error "Agenda source must use Org mode"))
        (unless (or (buffer-modified-p) (verify-visited-file-modtime (current-buffer)))
          (revert-buffer t t))
        (save-excursion
          (save-restriction
            (widen)
            (org-map-entries
             (lambda ()
               (when (and (org-get-todo-state) (not (org-in-archived-heading-p))
                          (not (org-in-commented-heading-p)))
                 (push (list :marker (copy-marker (point)) :file file
                             :title (org-get-heading t t t t) :state (org-get-todo-state)
                             :done (org-entry-is-done-p) :priority (org-entry-get nil "PRIORITY")
                             :scheduled (org-entry-get nil "SCHEDULED")
                             :deadline (org-entry-get nil "DEADLINE")
                             :todo-states (copy-sequence org-todo-keywords-1)
                             :done-states (copy-sequence org-done-keywords)
                             :tick (buffer-chars-modified-tick)) entries))) nil nil)))))
    (sort entries
          (lambda (a b)
            (let ((da (min (or (pebble-agenda--date (plist-get a :scheduled)) 9999999)
                           (or (pebble-agenda--date (plist-get a :deadline)) 9999999)))
                  (db (min (or (pebble-agenda--date (plist-get b :scheduled)) 9999999)
                           (or (pebble-agenda--date (plist-get b :deadline)) 9999999))))
              (if (= da db)
                  (string-lessp (concat (plist-get a :priority) (plist-get a :title))
                                (concat (plist-get b :priority) (plist-get b :title)))
                (< da db)))))))

(defun pebble-agenda--filtered ()
  (cl-remove-if-not
   (lambda (entry)
     (let ((date (min (or (pebble-agenda--date (plist-get entry :scheduled)) 9999999)
                      (or (pebble-agenda--date (plist-get entry :deadline)) 9999999))))
       (pcase pebble-agenda--view
         ('done (plist-get entry :done))
         ('open (not (plist-get entry :done)))
         (_ (and (not (plist-get entry :done))
                 (<= date (+ (pebble-agenda--today) (if (eq pebble-agenda--view 'upcoming) 6 0))))))))
   pebble-agenda--entries))

(defun pebble-agenda--short (text &optional limit)
  "Bound text to UTF-8 bytes, never splitting a character."
  (let ((text (replace-regexp-in-string "[\n\r\t]+" " " (or text ""))) (limit (or limit 63)))
    (while (> (string-bytes text) limit) (setq text (substring text 0 -1)))
    text))
(defun pebble-agenda--publish (title rows &optional subtitle)
  "Publish TITLE and ROWS of (label . action), assigning screen-local IDs."
  (let ((elements (list (list :id 1 :type "text" :x 8 :y 3 :width 184 :height 26
                              :value (pebble-agenda--short title))))
        (actions (make-hash-table)) (id 10) (y (if subtitle 58 34)))
    (when subtitle
      (setq elements (append elements (list (list :id 2 :type "text" :x 8 :y 29
                                                  :width 184 :height 26 :value (pebble-agenda--short subtitle))))))
    (dolist (row rows)
      (setq elements (append elements (list (append (list :id id :type "text" :x 8 :y y
                                                         :width 184 :height 30 :value (pebble-agenda--short (car row)))
                                                   (when (cdr row) '(:selectable t))))))
      (when (cdr row) (puthash id (cdr row) actions))
      (setq id (1+ id) y (+ y 32)))
    (pebble-show (list :children (vconcat elements)))
    (setq pebble-agenda--actions actions)))

(defun pebble-agenda--list ()
  (setq pebble-agenda--screen 'list pebble-agenda--detail nil)
  (let* ((entries (pebble-agenda--filtered)) (size (max 1 (min 40 pebble-agenda-page-size)))
         (pages (max 1 (ceiling (length entries) size))))
    (setq pebble-agenda--page (min (1- pages) (max 0 pebble-agenda--page)))
    (let* ((start (* size pebble-agenda--page))
           (slice (cl-subseq entries start (min (length entries) (+ start size))))
           (rows (mapcar (lambda (entry)
                           (cons (format "%s %s" (plist-get entry :state) (plist-get entry :title))
                                 (lambda () (pebble-agenda--show-detail entry)))) slice)))
      (unless rows (setq rows (list (cons "No matching tasks" nil))))
      (when (> pebble-agenda--page 0)
        (setq rows (append rows (list (cons "< Previous page" (lambda () (cl-decf pebble-agenda--page) (pebble-agenda--list)))))))
      (when (< (1+ pebble-agenda--page) pages)
        (setq rows (append rows (list (cons "Next page >" (lambda () (cl-incf pebble-agenda--page) (pebble-agenda--list)))))))
      (setq rows (append rows (list (cons "Views / refresh" #'pebble-agenda--views))))
      (pebble-agenda--publish (format "Agenda: %s" (capitalize (symbol-name pebble-agenda--view))) rows
                              (format "%d tasks | %d/%d" (length entries) (1+ pebble-agenda--page) pages)))))

(defun pebble-agenda--views ()
  (setq pebble-agenda--screen 'views)
  (pebble-agenda--publish
   "Agenda views"
   (append (mapcar (lambda (view)
                     (cons (capitalize (symbol-name view))
                           (lambda () (setq pebble-agenda--view view pebble-agenda--page 0)
                             (pebble-agenda-refresh t)))) '(today upcoming open done))
           (list (cons "Refresh" (lambda () (pebble-agenda-refresh t)))))))

(defun pebble-agenda--show-detail (entry)
  (setq pebble-agenda--screen 'detail pebble-agenda--detail entry)
  (pebble-agenda--publish
   (plist-get entry :title)
   (list (cons (concat "State: " (plist-get entry :state))
               (lambda () (pebble-agenda--states entry)))
         (cons (concat "Scheduled: " (or (plist-get entry :scheduled) "none"))
               (lambda () (pebble-agenda--dates entry 'scheduled)))
         (cons (concat "Deadline: " (or (plist-get entry :deadline) "none"))
               (lambda () (pebble-agenda--dates entry 'deadline)))
         (cons (concat "Priority: " (plist-get entry :priority))
               (lambda () (pebble-agenda--priorities entry)))
         (cons (concat "File: " (file-name-nondirectory (plist-get entry :file))) nil))
   "Select a field | Back: list"))

(defun pebble-agenda--states (entry)
  (setq pebble-agenda--screen 'action)
  (pebble-agenda--publish
   "Set TODO state"
   (mapcar (lambda (state) (cons state (lambda () (pebble-agenda--edit entry 'state state))))
           (plist-get entry :todo-states))))

(defun pebble-agenda--dates (entry field)
  (setq pebble-agenda--screen 'action)
  (pebble-agenda--publish
   (if (eq field 'scheduled) "Schedule task" "Set deadline")
   (append (mapcar (lambda (pair)
                     (let ((label (car pair))
                           (date (calendar-gregorian-from-absolute (+ (pebble-agenda--today) (cdr pair)))))
                       (cons label (lambda () (pebble-agenda--edit entry field
                                                                  (format "%04d-%02d-%02d" (nth 2 date) (car date) (nth 1 date)))))))
                   '(("Today" . 0) ("Tomorrow" . 1) ("In one week" . 7)))
           (list (cons "Clear date" (lambda () (pebble-agenda--edit entry field nil)))))))

(defun pebble-agenda--priorities (entry)
  (setq pebble-agenda--screen 'action)
  (let ((marker (plist-get entry :marker)) rows)
    (unless (marker-buffer marker) (user-error "Task is no longer available"))
    (with-current-buffer (marker-buffer marker)
      (unless (and (<= ?A org-priority-highest org-priority-lowest ?Z))
        (user-error "Watch priority editing requires letter priorities"))
      (cl-loop for priority from org-priority-highest to org-priority-lowest
               for value = (char-to-string priority)
               do (push (cons value (let ((v value)) (lambda () (pebble-agenda--edit entry 'priority v)))) rows)))
    (pebble-agenda--publish "Set priority" (nreverse rows))))

(defun pebble-agenda--edit (entry field value)
  "Apply an explicit edit only to the unchanged, configured source task."
  (let* ((marker (plist-get entry :marker)) (buffer (marker-buffer marker))
         (file (plist-get entry :file)))
    (unless (and buffer (member file (pebble-agenda--files))) (user-error "Task source is no longer enabled"))
    (with-current-buffer buffer
      (unless (and (equal (file-truename buffer-file-name) file)
                   (verify-visited-file-modtime buffer)
                   (= (buffer-chars-modified-tick) (plist-get entry :tick)))
        (user-error "Task changed; refresh before editing"))
      (when (or buffer-read-only (not (file-writable-p file))) (user-error "Task source is read-only"))
      (when (and pebble-agenda-save-changes (buffer-modified-p))
        (user-error "Save your Emacs edits before using watch actions"))
      (save-excursion
        (save-restriction
          (widen) (goto-char marker)
          (unless (and (org-at-heading-p) (equal (org-get-heading t t t t) (plist-get entry :title))
                       (equal (org-get-todo-state) (plist-get entry :state)))
            (user-error "Task identity changed; refresh"))
          (atomic-change-group
            ;; Suppress interactive logging prompts, but retain user's Org hooks/workflow.
            (let ((org-inhibit-logging 'note)
                  (org-log-done (and org-log-done 'time))
                  (org-todo-log-states nil) (org-log-repeat nil)
                  (org-log-reschedule nil) (org-log-redeadline nil))
              (pcase field
                ('state
                 (unless (member value org-todo-keywords-1) (user-error "TODO state no longer exists"))
                 ;; Observe the requested transition before Org can reset a
                 ;; repeating task to its next TODO occurrence.
                 (let ((changed nil))
                   (let ((org-after-todo-state-change-hook
                          (cons (lambda () (when (equal org-state value) (setq changed t)))
                                org-after-todo-state-change-hook)))
                     (org-todo value))
                   (unless changed (user-error "Org blocked this TODO change"))))
                ('scheduled (org-schedule (unless value '(4)) value))
                ('deadline (org-deadline (unless value '(4)) value))
                ('priority (org-priority (string-to-char value)))
                (_ (error "Unknown watch action"))))
            (when pebble-agenda-save-changes (save-buffer))))))
    (setq pebble-agenda--detail nil)
    (pebble-agenda-refresh t)))

(defun pebble-agenda--valid-event ()
  (unless (and (equal (plist-get pebble--event-context :session) pebble--session)
               (equal (plist-get pebble--event-context :revision) pebble--revision))
    (signal 'pebble-stale-event nil)))
(defun pebble-agenda--error (message)
  (setq pebble-agenda--screen 'error)
  (pebble-agenda--publish "Action unavailable"
                          (list (cons message nil) (cons "Refresh agenda" (lambda () (pebble-agenda-refresh t)))
                                (cons "Back to list" #'pebble-agenda--list))))
(defun pebble-agenda--select (id)
  (pebble-agenda--valid-event)
  (let ((action (gethash id pebble-agenda--actions)))
    (unless action (signal 'pebble-stale-event nil))
    (condition-case error (funcall action)
      (error (pebble-agenda--error (error-message-string error))))))
(defun pebble-agenda--back (_id)
  (pebble-agenda--valid-event)
  (if (and (eq pebble-agenda--screen 'action) pebble-agenda--detail)
      (pebble-agenda--show-detail pebble-agenda--detail)
    (pebble-agenda-refresh t)))

(defun pebble-agenda-refresh (&optional force)
  "Refresh file state.  FORCE returns to the list even if nothing changed."
  (interactive (list t))
  (let* ((entries (pebble-agenda--collect))
         (signature (mapcar (lambda (entry) (cl-loop for (key value) on entry by #'cddr
                                                   unless (eq key :marker) append (list key value))) entries)))
    (unless (and (not force) (equal signature pebble-agenda--signature))
      (setq pebble-agenda--entries entries pebble-agenda--signature signature)
      (pebble-agenda--list))))
(defun pebble-agenda--poll ()
  (when pebble-agenda-mode
    (condition-case error (pebble-agenda-refresh)
      (error (unless (eq pebble-agenda--screen 'error) (pebble-agenda--error (error-message-string error)))))))

(defun pebble-agenda--token ()
  (if (file-exists-p pebble-agenda-token-file)
      (with-temp-buffer (insert-file-contents-literally pebble-agenda-token-file) (string-trim (buffer-string)))
    (let ((token (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally "/dev/urandom" nil nil 32)
                   (secure-hash 'sha256 (current-buffer)))))
      (make-directory (file-name-directory pebble-agenda-token-file) t)
      (let ((previous (default-file-modes)))
        (unwind-protect
            (progn (set-default-file-modes #o600)
                   (with-temp-file pebble-agenda-token-file (insert token "\n")))
          (set-default-file-modes previous)))
      (set-file-modes pebble-agenda-token-file #o600)
      token)))

;;;###autoload
(defun pebble-agenda-pairing-info ()
  "Display the endpoint and token for the Pebble app's Settings page."
  (interactive)
  (unless pebble-agenda-mode (user-error "Enable pebble-agenda-mode first"))
  (with-help-window "*Pebble Agenda pairing*"
    (princ "In the Pebble phone app, open Pebble Agenda > Settings.\n\n")
    (princ (format "Emacs address: %s\nPairing token: %s\n\n" pebble-agenda-public-url pebble-auth-token))
    (princ "Keep the token private. Emacs must remain running.\n")))

(defun pebble-agenda--stop ()
  (when pebble-agenda--timer (cancel-timer pebble-agenda--timer) (setq pebble-agenda--timer nil))
  (when pebble-agenda--owns-server
    (pebble-stop)
    (pebble-show '(:children []))
    (setq pebble-agenda--entries nil pebble-agenda--actions nil
          pebble-agenda--detail nil pebble-agenda--signature nil)
    (dolist (pair pebble-agenda--old-handlers)
      (if (cdr pair) (puthash (car pair) (cdr pair) pebble--handlers) (remhash (car pair) pebble--handlers)))
    (setq pebble-auth-token pebble-agenda--old-token pebble-agenda--owns-server nil)))

;;;###autoload
(define-minor-mode pebble-agenda-mode
  "Expose your configured Org agenda to the paired Pebble watch."
  :global t :group 'pebble-agenda
  (if (not pebble-agenda-mode) (pebble-agenda--stop)
    (unless (and pebble-agenda--owns-server (process-live-p pebble--server))
     (condition-case error
        (progn
          (when (process-live-p pebble--server) (user-error "Another Pebble server is running"))
          (unless (memq pebble-agenda-default-view '(today upcoming open done)) (user-error "Invalid agenda view"))
          (setq pebble-agenda--old-token pebble-auth-token
                pebble-agenda--old-handlers (mapcar (lambda (event) (cons event (gethash event pebble--handlers))) '("select" "back"))
                pebble-agenda--owns-server t pebble-auth-token (pebble-agenda--token))
          (unless (>= (length pebble-auth-token) 32) (user-error "Pairing token is too short"))
          (pebble-new-session)
          (setq pebble-agenda--view pebble-agenda-default-view pebble-agenda--page 0 pebble-agenda--signature nil)
          (pebble-agenda-refresh t)
          (pebble-on "select" #'pebble-agenda--select)
          (pebble-on "back" #'pebble-agenda--back)
          (pebble-start)
          (setq pebble-agenda--timer (run-at-time (max 1 pebble-agenda-refresh-seconds)
                                                 (max 1 pebble-agenda-refresh-seconds) #'pebble-agenda--poll)))
      (error (setq pebble-agenda-mode nil) (pebble-agenda--stop) (signal (car error) (cdr error)))))))

(provide 'pebble-agenda)
;;; pebble-agenda.el ends here
