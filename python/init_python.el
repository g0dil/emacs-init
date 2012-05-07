(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
(add-hook 'python-mode-hook
      (lambda ()
        (set-variable 'py-indent-offset 4)
        ;(set-variable 'py-smart-indentation nil)
        (set-variable 'indent-tabs-mode nil)
        (define-key py-mode-map (kbd "RET") 'newline-and-indent)
        ;(define-key py-mode-map [tab] 'yas/expand)
        ;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
        ;(smart-operator-mode-on)
        ))
;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;; M-/               rope-code-assist
;; C-c g             rope-goto-definition
;; C-c d             rope-show-doc
;; C-c f             rope-find-occurrences
;; M-?               rope-lucky-assist

(define-key ropemacs-local-keymap "\M-/" 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
      (setq value (cons (format "%s%s" prefix element) value))))))
(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")
(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))
(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))
(add-hook 'python-mode-hook
          (lambda ()
                 (auto-complete-mode 1)
                 (set (make-local-variable 'ac-sources)
                      (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
                 (set (make-local-variable 'ac-find-function) 'ac-python-find)
                 (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
                 (set (make-local-variable 'ac-auto-start) nil)))
;;Ryan's python specific tab completion
(defun ryan-python-tab ()
  ; Try the following:
  ; 1) Do a yasnippet expansion
  ; 2) Do a Rope code completion
  ; 3) Do an indent
  (interactive)
  (if (eql (ac-start) 0)
      (indent-for-tab-command)))
(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))
(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))
;(define-key py-mode-map "\t" 'ryan-python-tab)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Auto Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Syntax Error Hightlight
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun py-cleanup-imports (&optional nowait)
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (goto-char (point-min))
        (while (and (re-search-forward "^\\(import\\s-+\\|from\\s-+\\)" nil t)
                    (looking-at "__future__")))
        (beginning-of-line)
        (if (not (looking-at "\\(import\\s-+\\|from\\s-+\\)"))
            (error "No imports found"))
        (setq beg (point))
        (while (looking-at "\\(import\\s-+\\|from\\s-+\\)")
          (forward-line 1))
        (setq end (point))))
    (sort-lines t beg end)
    (goto-char beg)
    (let ((end-marker (make-marker))
          (doing-imports t)
          (b beg))
      (set-marker end-marker end)
      (while (< (point) end-marker)
        (let ((is-import (looking-at "import\\s-+"))
              (eol-marker (save-excursion (end-of-line) (point-marker)))
              prefix)
          (if (and doing-imports (not is-import))
              (progn
                (if (> (point) b)
                    (progn
                      (sort-lines nil beg (point))
                      (setq b (point))))
                (setq doing-imports nil)))
          (setq prefix (if is-import "import "
                         (buffer-substring-no-properties
                          (point) (re-search-forward "\\s-+import\\s-+" eol-marker t))))
          (while (search-forward "," eol-marker t)
            (delete-char -1)
            (delete-horizontal-space)
            (insert "\n" prefix))
          (forward-line 1)))
      (sort-lines nil b (point))
      (if (and (not nowait) (boundp flymake-is-running))
          (progn
            (while flymake-is-running (sit-for .1))
            (flymake-start-syntax-check)
            (while flymake-is-running (sit-for .1))
            (goto-char beg)
            (while (< (point) end-marker)
              (if (and (loop for ov in (overlays-at (point))
                             thereis (flymake-overlay-p ov))
                       (not (save-excursion (search-forward "unused"
                                                            (save-excursion (end-of-line) (point))
                                                            t ))))
                  (delete-region (point)
                                 (progn (forward-line 1) (point)))
                (forward-line 1))))))))

(defun flyspell-py-progmode-verify ()
  "Replacement for standard flyspell-generic-progmode-verify which
checks for C/C++ preproc directives. Additionally, anything after ^L
is ignored (Those are the file local variables and local words)."
  (let ((f (get-text-property (point) 'face)))
    (and (memq f flyspell-prog-text-faces)
         (not (save-excursion
                (beginning-of-line)
                (looking-at "#!")))
         (not (let ((l (max (point-min) (- (point-max) 4096))))
                (and (< l (point))
                     (save-excursion (search-backward "" l t)))))
         (not (let* ((pos (python-in-string/comment))
                     (c (and pos (char-after pos))))
                (and pos (not (save-excursion
                                (goto-char pos)
                                (beginning-of-line)
                                (looking-at (concat "^\\s-*[uUrR]?" (regexp-quote (make-string 3 c))))))))))))

(defun flyspell-py-mode ()
  "Turn on `flyspell-mode` for comments and multi-line strings"
  (interactive)
  (setq flyspell-generic-check-word-p 'flyspell-py-progmode-verify)
  (flyspell-mode t))

(provide 'init_python)
