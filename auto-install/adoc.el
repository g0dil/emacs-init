(require 'adoc-mode)

(defun adoc-indent (&optional n)
  (interactive)
  (setq n (or n 1))
  (let* ((ccol (current-column))
         (cind (current-indentation))
         (nind (* tab-width (+ (/ cind tab-width) n)))
         (ncol (if (<= ccol cind) nind (+ ccol (- nind cind)))))
    (if (or (> n 0) (> cind 0))
      (progn
        (indent-line-to nind)
        (move-to-column ncol t)))))

(defun adoc-unindent ()
  (interactive)
  (adoc-indent -1))

(defun adoc-setup()
  (setq-local tab-width 2)
  (flyspell-mode 1)
  (auto-fill-mode 1))

(add-hook 'adoc-mode-hook 'adoc-setup)

; I don't set indent-line-function since that is used in other places. Instead, I just rebind the
; tab and backtab keys
(define-key adoc-mode-map "\t" 'adoc-indent)
(define-key adoc-mode-map (kbd "<backtab>") 'adoc-unindent)
