(require 'bookmark+)

(defun g0dil-goto-equivalent-position (other-file)
  (let ((bk (bookmark-make-record-default t)))
    (setq bk (bookmark-set-filename bk other-file))
    (bookmark-jump (cons "" bk))))
