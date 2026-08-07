;;; -*- lexical-binding: t -*-

;; Make emacs on Android work with Termux
(setenv "PATH"
		(format
		 "%s:%s:%s"
		 "/data/data/org.gnu.emacs/files/bin/"
		 "/data/data/com.termux/files/usr/bin"
		 (getenv "PATH")))
(push "/data/data/org.gnu.emacs/files/bin" exec-path)
(push "/data/data/com.termux/files/usr/bin" exec-path)
