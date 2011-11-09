(add-to-list 'auto-mode-alist '("\\.[Xx][Qq][Uu][Ee][Rr][Yy]\\'" . xquery-mode))

(defvar dbxml-shell-filename "dbxml")

(defun dbxml ()
  (interactive)
  (pop-to-buffer (make-comint "DB-Xml" dbxml-shell-filename))
  (set-buffer-process-coding-system "utf-8" "utf-8"))
