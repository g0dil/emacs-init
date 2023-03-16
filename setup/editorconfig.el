(editorconfig-mode 1)

(defun my-editorconfig-invalidate-cache ()
  (interactive)
  (setq editorconfig-core-handle--cache-hash (make-hash-table :test 'equal)))
