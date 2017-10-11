(let ((base (file-name-directory
             (directory-file-name
              (file-name-directory
               (or load-file-name
                   (when (boundp 'bytecomp-filename) bytecomp-filename) buffer-file-name))))))

  (add-to-list 'load-path (concat base "lib"))
  (add-to-list 'load-path (concat base "auto-install"))
  (setq custom-file (concat base "emacs-custom.el")))

(setenv "LANG" nil)
(prefer-coding-system 'utf-8)
(menu-bar-mode -1)
