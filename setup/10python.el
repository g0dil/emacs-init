(let ((dir (concat (file-name-directory
                    (directory-file-name
                     (file-name-directory
                      (or load-file-name
                          (when (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))))
                   "python/")))
  (load (concat dir "setup.el")))
