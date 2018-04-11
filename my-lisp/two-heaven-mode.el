;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

(defvar two-heaven-mode-map (make-sparse-keymap)
  "Keymap for 'two-heaven-mode'.")

;;;###autoload
(define-minor-mode two-heaven-mode
  "A minor mode to override keybinds with THS."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; 'fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " Two-Heaven"
  :keymap two-heaven-mode-map)

;;;###autoload
(define-globalized-minor-mode global-two-heaven-mode two-heaven-mode two-heaven-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((two-heaven-mode . ,two-heaven-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off two-heaven-mode."
  (two-heaven-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)

(provide 'two-heaven-mode)

;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/

;; Preceding code adapted from https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings/358#358

(define-key two-heaven-mode-map (kbd "C-/") 'help-command)
(define-key two-heaven-mode-map (kbd "C-s") 'save-buffer)
(define-key two-heaven-mode-map (kbd "C-f") 'isearch-forward)
(define-key two-heaven-mode-map (kbd "C-l") 'forward-char) ;;Leaves recenter-top-bottom unbound
(define-key two-heaven-mode-map (kbd "C-k") 'previous-line) ;;Leaves kill-line unbound
(define-key two-heaven-mode-map (kbd "C-j") 'next-line) ;;Leaves electric-newline-and-maybe-indent unbound
(define-key two-heaven-mode-map (kbd "C-h") 'backward-char)
(define-key two-heaven-mode-map (kbd "C-z") 'undo) ;;Leaves suspend-frame unbound
(define-key two-heaven-mode-map (kbd "C-S-z") 'redo)
(setq help-char nil)
(define-key two-heaven-mode-map (kbd "C-/") 'help-command)
(define-key two-heaven-mode-map (kbd "C-?") 'help-command)
(define-key two-heaven-mode-map (kbd "M-x") 'helm-M-x)
