(global-set-key "\M-/" 'hippie-expand)
(global-set-key "\C-c'w" 'which-function-mode)
(global-set-key "\C-c'h" 'highlight-changes-mode)
(global-set-key "\C-c'f" 'auto-fill-mode)
(global-set-key "\C-c'v" 'visual-line-mode)
(global-set-key "\C-c't" 'toggle-truncate-lines)
(global-set-key "\C-c'd" 'toggle-debug-on-error)
(global-set-key "\C-c'g" 'toggle-debug-on-quit)
(global-set-key "\C-c'c" 'toggle-case-fold-search)
(global-set-key "\C-c's" 'flyspell-mode)
(global-set-key "\C-c'S" 'flyspell-prog-mode)
(global-set-key "\C-c'a" 'global-auto-revert-mode)
(global-set-key "\C-cf" 'ffap)

(defun g0dil-fixed-font ()
  (interactive)
  (set-frame-font "6x13"))

(defun g0dil-scalable-font()
  (interactive)
  (set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

(global-set-key "\C-cFf" 'g0dil-fixed-font)
(global-set-key "\C-cFs" 'g0dil-scalable-font)
(global-set-key "\C-cFm" 'menu-set-font)

;(require 'develock)
;(require 'develock-py)

(defun toggle-whitespace-mode (&optional mode)
  (interactive)
  ;; toggle whitespace mode between:
  ;; * everything as globaly configured
  ;; * as above but disable lines-tail
  ;; * nothing
  ;; * as above but visualize tabs

  ;; if mode is not set, detect current mode and set it to the next value in the list
  (if (null mode)
      (if (local-variable-p 'whitespace-style)
          (if (null whitespace-style)
              (setq mode 'tabs)
            (if (memq 'tab-mark whitespace-style)
                (setq mode 'default)
              (setq mode 'none)))
        (setq mode 'longlines)))
  (cond ((eq mode 'default)
         (kill-local-variable 'whitespace-style))
        ((eq mode 'longlines)
         (setq-local whitespace-style (remove 'lines-tail (default-value 'whitespace-style))))
        ((eq mode 'tabs)
         (setq-local whitespace-style '(tab-mark)))
        ((eq mode 'none)
         (setq-local whitespace-style nil)))
  (whitespace-mode -1)
  (sit-for 0)
  (whitespace-mode 1)
  (message "whitespace: %s" mode))

(global-set-key "\C-c' " 'toggle-whitespace-mode)

(setq confirm-kill-emacs 'yes-or-no-p)

(defun kill-buffers-in-directory (directoryName &optional includeNonFiles)
  (interactive (list
                (ido-read-directory-name
                 (if current-prefix-arg "Kill buffers in directory:" "Kill files in directory: "))
                current-prefix-arg))
  (setq directoryName (expand-file-name directoryName))
  (if (not (string-suffix-p "/" directoryName))
    (setq directoryName (concat directoryName "/")))
  (loop for buffer in (buffer-list)
        for bufferDirectory = (progn
                                (set-buffer buffer)
                                (if includeNonFiles default-directory
                                    (and buffer-file-name (file-name-directory buffer-file-name))))
        if (and bufferDirectory (string-prefix-p directoryName (expand-file-name bufferDirectory)))
        do (kill-buffer buffer)))

(global-set-key "\C-xK" 'kill-buffers-in-directory)

(global-unset-key "\C-z")
(global-unset-key "\C-xz")

(global-set-key "\C-xA" 'org-agenda)
