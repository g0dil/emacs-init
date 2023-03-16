;;; datetime-format-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "datetime-format" "datetime-format.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from datetime-format.el

(autoload 'datetime-format "datetime-format" "\
Use SYM-OR-FMT to format the time TIME and OPTION plist.

OPTION plist expect :timezone.
See URL `https://en.wikipedia.org/wiki/List_of_tz_database_time_zones'

\(fn SYM-OR-FMT &optional TIME &rest OPTION)" nil nil)

(register-definition-prefixes "datetime-format" '("datetime-format-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; datetime-format-autoloads.el ends here
