(let ((dir (concat (file-name-directory
                    (directory-file-name
                     (file-name-directory (or load-file-name
                                         (when (boundp 'bytecomp-filename) bytecomp-filename)
                                         buffer-file-name))))
                   (file-name-as-directory "company-mode"))))
  (add-to-list 'load-path dir))

(require 'company)
