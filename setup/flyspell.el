; (require 'advice)

; Speed up interactivity in flyspell (it's unusable on windows otherwise)

; (defadvice flyspell-check-pre-word-p (around flyspell-delay-all activate)
;   (setq ad-return-value nil))

(require 'flyspell)

(defun my-flyspell-accept-word-buffer-local ()
  (interactive)
  (let ((word (flyspell-get-word)))
    (flyspell-do-correct 'buffer '(*) (car word) (point) (car (cdr word)) (car (cdr (cdr word)))
                         (point)))
  (sit-for .1)
  (flyspell-auto-correct-word))

(define-key flyspell-mode-map (kbd "C-,") 'my-flyspell-accept-word-buffer-local)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-goto-next-error)
