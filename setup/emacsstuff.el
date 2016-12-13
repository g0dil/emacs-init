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

;;; remove some unnecessary compilation error regexp settings which just cause problems
(setq compilation-error-regexp-alist (delq 'cucumber compilation-error-regexp-alist))
(setq compilation-error-regexp-alist (delq 'mips-1 compilation-error-regexp-alist))

;;; remove matching whitespace in filenames for 'gnu' style error messages
(setf (nth 1 (assoc 'gnu compilation-error-regexp-alist-alist))
      (replace-regexp-in-string (regexp-quote " [^-/\n]\\|") ""
                                (nth 1 (assoc 'gnu compilation-error-regexp-alist-alist)) t t))

(defun debug-compilation-error-regexps ()
  (interactive)
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (narrow-to-region (point) (save-excursion (forward-line 1) (point)))
      (let ((match (loop for key in compilation-error-regexp-alist
                         for elt = (assoc key compilation-error-regexp-alist-alist)
                         if (and elt (re-search-forward (cadr elt) nil t))
                         thereis key)))
        (if match
            (message "compilation-error-regexp-alist-alist key: %s" match)
          (error "no error line at point"))))))
