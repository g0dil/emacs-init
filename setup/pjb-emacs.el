(let ((pjb-libdir (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "pjb-emacs")))
  (add-to-list 'load-path pjb-libdir)
  (loop for feature in '(pjb-cl-magic pjb-sources pjb-strings pjb-list pjb-cl pjb-utilities)
        do (require feature))
  (load-file (concat (file-name-as-directory pjb-libdir) "pjb-emacs-balance-windows.el")))
