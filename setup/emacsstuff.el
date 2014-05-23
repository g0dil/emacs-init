(let ((dir (concat (file-name-directory
                    (directory-file-name
                     (file-name-directory (or load-file-name
                                         (when (boundp 'bytecomp-filename) bytecomp-filename)
                                         buffer-file-name))))
                   (file-name-as-directory "emacsstuff"))))
  (add-to-list 'load-path dir))

(require 'g0dilstuff-init)
(require 'cc-ide)
(require 'softtext)
(require 'cc-mode)

(define-key c++-mode-map "\C-c\C-c" 'recompile)
(define-key c++-mode-map "\C-c\C-i" 'comment-indent)
