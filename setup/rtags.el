(let ((base (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))))
  (add-to-list 'load-path (concat base
                                  (file-name-as-directory "rtags")
                                  (file-name-as-directory "src")))
  (add-to-list 'load-path (concat base
                                  (file-name-as-directory "elpa")
                                  (file-name-as-directory "company-0.9.2")))

  (setq rtags-path (concat base
                           (file-name-as-directory "rtags")
                           (file-name-as-directory "build")
                           (file-name-as-directory "bin"))))

(require 'rtags)

;; ;; ensure that we use only rtags checking
;; ;; https://github.com/Andersbakken/rtags#optional-1
;; (defun setup-flycheck-rtags ()
;;   (interactive)
;;   (flycheck-select-checker 'rtags)
;;   ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-highlighting-mode nil)
;;   (setq-local flycheck-check-syntax-automatically nil))

;; only run this if rtags is installed
(when (require 'rtags nil :noerror)
  ;; make sure you have company-mode installed
  (require 'company)
  (define-key c-mode-base-map (kbd "\C-c.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "\C-c,")
    (function rtags-find-references-at-point))
  (define-key c-mode-base-map (kbd "\C-cn")
    (function rtags-next-diag))
  ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
  ;; (define-key prelude-mode-map (kbd "C-c r") nil)
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; comment this out if you don't have or don't use helm
  (setq rtags-use-helm t)
  ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
  (global-company-mode)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;; use rtags flycheck mode -- clang warnings shown inline
  ;; (require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  ;; (add-hook 'c-mode-common-hook #'setup-flycheck-rtags)
  )
