(let ((dir (concat (file-name-directory
                    (directory-file-name
                     (file-name-directory
                      (or load-file-name
                          (when (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))))
                   "git-wip/")))
  (load (concat dir "emacs/git-wip-mode.el"))
  (add-to-list 'exec-path dir)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/lib/git-core")))
