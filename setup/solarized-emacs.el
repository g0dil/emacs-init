(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "solarized-emacs"))

(setq solarized-use-less-bold t)
(setq solarized-distinct-fringe-background t)
(require 'solarized-light-theme)
