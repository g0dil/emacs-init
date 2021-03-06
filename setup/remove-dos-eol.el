(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])
  (force-window-update (current-buffer))
  (redisplay 't))

(global-set-key "\C-cM" 'remove-dos-eol)
