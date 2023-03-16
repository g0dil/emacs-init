(let ((dir (concat (file-name-directory
                    (directory-file-name
                     (file-name-directory (or load-file-name
                                         (when (boundp 'bytecomp-filename) bytecomp-filename)
                                         buffer-file-name))))
                   (file-name-as-directory "yaml-mode"))))
  (add-to-list 'load-path dir))
(let ((dir (concat (file-name-directory
                    (directory-file-name
                     (file-name-directory (or load-file-name
                                         (when (boundp 'bytecomp-filename) bytecomp-filename)
                                         buffer-file-name))))
                   (file-name-as-directory "openapi-yaml-mode"))))
  (add-to-list 'load-path dir))

(require 'yaml-mode)
(require 'openapi-yaml-mode)
(require 'docker-compose-mode)

(defvar my-yaml-indent-shift-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" 'my-yaml-indent-shift-left)
    (define-key map ">" 'my-yaml-indent-shift-right)
    map))

(defun my-yaml-indent-shift-left (start end &optional count interactive)
  (interactive
   (if mark-active
     (list (region-beginning) (region-end) current-prefix-arg t)
     (list (line-beginning-position) (line-end-position) current-prefix-arg t)))
  (if (and (not count) interactive)
    (progn
      (message "Shift region with '<' and '>'")
      (set-transient-map my-yaml-indent-shift-map t 'deactivate-mark)))
  (setq count (if count (* (prefix-numeric-value count) yaml-indent-offset) yaml-indent-offset))
  (python-indent-shift-left start end count))

(defun my-yaml-indent-shift-right (start end &optional count interactive)
  (interactive
   (if mark-active
     (list (region-beginning) (region-end) current-prefix-arg t)
     (list (line-beginning-position) (line-end-position) current-prefix-arg t)))
  (if (and (not count) interactive)
    (progn
      (message "Shift region with '<' and '>'")
      (set-transient-map my-yaml-indent-shift-map t 'deactivate-mark)))
  (setq count (if count (* (prefix-numeric-value count) yaml-indent-offset) yaml-indent-offset))
  (python-indent-shift-right start end count))

(define-key yaml-mode-map "\C-c<" 'my-yaml-indent-shift-left)
(define-key yaml-mode-map "\C-c>" 'my-yaml-indent-shift-right)
