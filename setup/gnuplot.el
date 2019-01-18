(let ((dir (concat (file-name-directory
                    (directory-file-name
                     (file-name-directory (or load-file-name
                                         (when (boundp 'bytecomp-filename) bytecomp-filename)
                                         buffer-file-name))))
                   (file-name-as-directory "gnuplot"))))
  (add-to-list 'load-path dir)
  (load (concat dir "load.el")))

(defun gnuplot ()
  (interactive)
  (gnuplot-make-gnuplot-buffer)
  (sit-for .1)
  (switch-to-buffer "*gnuplot*"))
