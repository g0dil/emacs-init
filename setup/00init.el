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

(defun make-obsolete-variable-opt-when (orig obsolete-name current-name &optional when &rest args)
  (apply orig obsolete-name current-name (or when "undefined") args))

(advice-add 'make-obsolete-variable :around 'make-obsolete-variable-opt-when)

(defun make-obsolete-opt-when (orig obsolete-name current-name &optional when &rest args)
  (apply orig obsolete-name current-name (or when "undefined") args)
)
(advice-add 'make-obsolete :around 'make-obsolete-opt-when)
