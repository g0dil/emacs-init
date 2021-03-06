(require 'esh-mode)

; grrr ... eshell-mode-map is buffer-local ...
(defun my-setup-eshell ()
  (define-key eshell-mode-map (kbd "<up>") 'previous-line)
  (define-key eshell-mode-map (kbd "<down>") 'next-line)
  (define-key eshell-mode-map (kbd "C-<up>") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "C-<down>") 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-bol))

(add-hook 'eshell-mode-hook 'my-setup-eshell)

(defun eshell-switch-directory-current-buffer ()
  (interactive)
  (let ((dir default-directory)
        (eshell-buffer (get-buffer "*eshell*")))
    (if (not eshell-buffer)
        (error "no *eshell* buffer found"))
    (my-pop-to-buffer (get-buffer "*eshell*"))
    (goto-char (point-max))
    (setq default-directory dir)
    (eshell-interrupt-process)))

(global-set-key "\C-cE" 'eshell-switch-directory-current-buffer)

(setenv "PATH" (concat (getenv "PATH")
                       ":"
                       (expand-file-name "~/bin")
                       ":"
                       (expand-file-name "~/.local/bin")))
