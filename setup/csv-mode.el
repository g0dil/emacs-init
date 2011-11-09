(require 'csv-mode)

(defun csv-set-separator (value)
  (interactive "cSeparator char <press key> ?")
  (csv-unalign-fields nil (point-min) (point-max))
  (set (make-variable-buffer-local 'csv-separators) (list (char-to-string value)))
  (set (make-variable-buffer-local 'csv-separator-chars) (list value))
  (set (make-variable-buffer-local 'csv-skip-regexp) (apply 'concat "^\n" csv-separators))
  (set (make-variable-buffer-local 'csv-separator-regexp) (apply 'concat `("[" ,@csv-separators "]")))
  (set (make-variable-buffer-local 'csv-font-lock-keywords) `((,csv-separator-regexp . csv-separator-face)))
  (csv-align-fields nil (point-min) (point-max))
  (font-lock-refresh-defaults)
  (font-lock-fontify-buffer))

(defun csv-mode-init ()
  (set (make-variable-buffer-local 'truncate-lines) t)
  (csv-align-fields nil (point-min) (point-max)))

(define-key csv-mode-map "\C-c;" 'csv-set-separator)
(define-key csv-mode-map "\C-ch" 'dpservice-csv-insert-header) ;; siehe dpservice-dbspec.el

(add-to-list 'auto-mode-alist '("\\.[Aa][Ss][Cc]\\'" . csv-mode))
(add-hook 'csv-mode-hook 'csv-mode-init)
