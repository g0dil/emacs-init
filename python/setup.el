(let ((self-dir (file-name-directory (or load-file-name
                                         (when (boundp 'bytecomp-filename) bytecomp-filename)
                                         buffer-file-name))))
  (add-to-list 'load-path self-dir))

(load-library "auto-completion")

(require 'init_python)

(defun write-file-py-cleanup-imports ()
  (save-excursion
    (condition-case nil
        (py-cleanup-imports)
      (error . nil)))
  nil)

(defun python-init-auto-cleanup-imports-on-save ()
  (add-hook 'write-file-functions 'write-file-py-cleanup-imports nil t))

(add-hook 'python-mode-hook 'python-init-auto-cleanup-imports-on-save)
