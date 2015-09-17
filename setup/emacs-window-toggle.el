(let ((dir (concat (file-name-directory
                    (directory-file-name
                     (file-name-directory
                      (or load-file-name
                          (when (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))))
                   "emacs-window-toggle/")))
  (load (concat dir "emacs-window-toggle.el"))
  (global-set-key "\C-c\C-\\" 'toggle-windows-split))
