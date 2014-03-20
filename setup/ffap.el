(defun my-find-file-at-point-with-line ()
  "Opens the file at point and goes to line-number."
  (interactive)
  (let ((fname (ffap-file-at-point)))
    (if fname
      (let ((line
             (save-excursion
               (goto-char (cadr ffap-string-at-point-region))
               (and (re-search-backward ":\\([0-9]+\\)"
                                        (line-beginning-position) t)
                    (string-to-int (match-string 1))))))
        ;; (message "file:%s,line:%s" fname line)
        (when (and (tramp-tramp-file-p default-directory)
                   (= ?/ (aref fname 0)))
          ;; if fname is an absolute path in remote machine, it will not return a tramp path,fix it here.
          (let ((pos (position ?: default-directory)))
            (if (not pos) (error "failed find first tramp indentifier ':'"))
            (setf pos (position ?: default-directory :start (1+ pos)))
            (if (not pos) (error "failed find second tramp indentifier ':'"))
            (setf fname (concat (substring default-directory 0 (1+ pos)) fname))))
        (message "fname:%s" fname)
        (find-file-existing fname)
        (when line (goto-line line)))
      (error "File does not exist."))))

(global-set-key (kbd "C-c C-x C-f") 'my-find-file-at-point-with-line)