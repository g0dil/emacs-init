(global-set-key "\M-/" 'hippie-expand)
(global-set-key "\C-c'w" 'which-func-mode)
(global-set-key "\C-c'h" 'highlight-changes-mode)
(global-set-key "\C-c'f" 'auto-fill-mode)
(global-set-key "\C-c'v" 'visual-line-mode)
(global-set-key "\C-c't" 'toggle-truncate-lines)
(global-set-key "\C-c'd" 'toggle-debug-on-error)
(global-set-key "\C-c'g" 'toggle-debug-on-quit)
(global-set-key "\C-c'c" 'toggle-case-fold-search)
(global-set-key "\C-c's" 'flyspell-mode)
(global-set-key "\C-c'a" 'global-auto-revert-mode)

;(require 'develock)
;(require 'develock-py)

(defun toggle-whitespace-mode (&optional mode)
  (interactive)
  ;; toggle whitespace mode between:
  ;; * everything as globaly configured
  ;; * as above but disable lines-tail
  ;; * nothing
  (if (null mode)
      (if (local-variable-p 'whitespace-style)
          (if (null whitespace-style)
              (setq mode 'default)
            (setq mode 'none))
        (setq mode 'longlines)))
  (cond ((eq mode 'default)
         (kill-local-variable 'whitespace-style))
        ((eq mode 'longlines)
         (setq-local whitespace-style (remove 'lines-tail (default-value 'whitespace-style))))
        ((eq mode 'none)
         (setq-local whitespace-style nil)))
  (whitespace-mode -1)
  (sit-for 0)
  (whitespace-mode 1)
  (message "whitespace: %s" mode))

(global-set-key "\C-c' " 'toggle-whitespace-mode)
