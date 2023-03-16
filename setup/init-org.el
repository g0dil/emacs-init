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
  (require 'org-mouse)

  (setq org-odt-data-dir (concat base "org-mode/etc")))

(defun my-setup-org ()
  (visual-line-mode 1)
  (font-lock-mode 0)
  (whitespace-mode 0)
  (font-lock-mode 1))

(add-hook 'org-mode-hook 'my-setup-org)

(define-key org-mode-map (kbd "S-<left>") nil)
(define-key org-mode-map (kbd "S-<right>") nil)
(define-key org-mode-map (kbd "S-<down>") nil)
(define-key org-mode-map (kbd "S-<up>") nil)

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(define-key org-mode-map "\C-cA" 'org-archive-done-tasks)
