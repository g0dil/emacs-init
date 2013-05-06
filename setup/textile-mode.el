(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "textile-mode"))

(require 'textile-mode)

(add-to-list 'auto-mode-alist '("/itsalltext/otds.traveltainment.de" . textile-mode))
(add-to-list 'edit-server-url-major-mode-alist '("tpm.traveltainment.int" . textile-mode))

(setq textile-font-lock-keywords
      (delq (loop for elt in textile-font-lock-keywords
                  for face = (nth 2 elt)
                  if (and (consp face)
                          (eq (car face) 'quote)
                          (eq (cadr face) 'textile-acronym-face)) return elt)
            textile-font-lock-keywords))
