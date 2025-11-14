;;; -*- lexical-binding: t -*-

;; Make emacs on Android work with Termux
(setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		       (getenv "PATH")))
(push "/data/data/com.termux/files/usr/bin" exec-path)
