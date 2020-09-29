(let ((libdir (concat (file-name-directory
                       (directory-file-name
                        (file-name-directory
                         (or load-file-name
                             (when (boundp 'bytecomp-filename) bytecomp-filename)
                             buffer-file-name))))
                      "pyvenv")))
  (add-to-list 'load-path libdir)
  (require 'pyvenv))

(pyvenv-mode 1)
(pyvenv-tracking-mode 1)
