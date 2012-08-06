(let ((self-dir (file-name-directory (or load-file-name
                                         (when (boundp 'bytecomp-filename) bytecomp-filename)
                                         buffer-file-name))))
  (add-to-list 'load-path self-dir))

(load-library "auto-completion")

(require 'init_python)
