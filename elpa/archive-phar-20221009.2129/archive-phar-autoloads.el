;;; archive-phar-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "archive-phar" "archive-phar.el" (0 0 0 0))
;;; Generated autoloads from archive-phar.el

(autoload 'archive-phar-find-type "archive-phar" "\
Added logic to `archive-find-type' to detect Phar." nil nil)

(with-eval-after-load "arc-mode" (advice-add #'archive-find-type :before-until #'archive-phar-find-type))

(add-to-list 'auto-mode-alist '("\\.phar\\'" . archive-mode))

(register-definition-prefixes "archive-phar" '("archive-phar-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; archive-phar-autoloads.el ends here
