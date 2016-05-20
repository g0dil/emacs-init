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

(defun toggle-whitespace-modes ()
  (interactive)
  (if whitespace-mode
      (progn
        (whitespace-mode 0)
        ;(develock-mode 0)
        )
    (whitespace-mode 1)
    ;(develock-mode 1)
    )
  ; for some reason, the font-lock information is only updated when running normal-mode again
  (normal-mode))

(global-set-key "\C-c' " 'toggle-whitespace-modes)
