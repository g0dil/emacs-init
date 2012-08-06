(require 'esh-mode)

; grrr ... eshell-mode-map is buffer-local ...
(defun my-setup-eshell ()
  (define-key eshell-mode-map (kbd "<up>") 'previous-line)
  (define-key eshell-mode-map (kbd "<down>") 'next-line)
  (define-key eshell-mode-map (kbd "C-<up>") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "C-<down>") 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-bol))

(add-hook 'eshell-mode-hook 'my-setup-eshell)
