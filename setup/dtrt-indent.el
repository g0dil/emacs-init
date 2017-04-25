(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "dtrt-indent"))

(require 'dtrt-indent)
