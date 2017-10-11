(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "solarized-emacs"))

(setq solarized-distinct-fringe-background t)
;(setq solarized-use-less-bold nil)
(require 'solarized-light-theme)
;(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
