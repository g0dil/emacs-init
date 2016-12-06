(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "groovy-emacs-modes"))
(require 'groovy-mode)
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))