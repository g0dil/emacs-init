(let ((base (file-name-directory
             (directory-file-name
              (file-name-directory
               (or load-file-name
                   (when (boundp 'bytecomp-filename) bytecomp-filename) buffer-file-name))))))

  (add-to-list 'load-path (concat base "org-mode/lisp"))
  (add-to-list 'load-path (concat base "org-mode/contrib"))

  (require 'flyspell)
  (if (not (boundp 'flyspell-delayed-commands))
      (setq flyspell-delayed-commands '()))

  (require 'org)
  (require 'org-version)

  (setq org-odt-data-dir (concat base "org-mode/etc")))

(defun my-setup-org ()
  (visual-line-mode 1)
  (font-lock-mode 0)
  (whitespace-mode 0)
  (font-lock-mode 1))

(add-hook 'org-mode-hook 'my-setup-org)