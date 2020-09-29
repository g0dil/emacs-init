;(autoload 'python-mode "python-mode" "Python Mode." t)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;(require 'python-mode)
;(provide 'python)
;(provide 'python21)

(global-unset-key "\C-xpn")

(require 'python)

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

(defun write-file-py-cleanup-imports ()
  (save-excursion
    (condition-case nil
        (py-cleanup-imports t)
      (error . nil)))
  nil)

(defun python-init-auto-cleanup-imports-on-save ()
  (add-hook 'write-contents-functions 'write-file-py-cleanup-imports nil t))

(defun my-flymake-error-at-point ()
  (condition-case  nil
      (flymake-ler-text (car (nth 0 (flymake-find-err-info flymake-err-info
                                                           (locate-current-line-number)))))
    (error (error "no flymake error at point"))))

(defun my-flymake-show-error ()
  (interactive)
  (message (my-flymake-error-at-point)))

(defun my-pyflymake-add-import-from-error ()
  (interactive)
  (let ((err (my-flymake-error-at-point)))
    (if (string-match "undefined name '\\(.*\\)'" err)
        (py-add-import (match-string 1 err))
      (error "no missing symbol at point"))))

(defun py-add-import (import)
  (interactive "sModule to import: ")
  (save-window-excursion
    (save-excursion
      (goto-char (car (py-imports-region)))
      (insert "import " import "\n")
      (py-cleanup-imports t)
      (sit-for 2))))

(defun my-flymake-check-and-wait ()
  (if (boundp flymake-is-running)
      (progn
        (while flymake-is-running (sit-for .1))
        (flymake-start-syntax-check)
        (while flymake-is-running (sit-for .1)))))

(defun my-flymake-goto-next-error ()
  (interactive)
  (my-flymake-check-and-wait)
  (flymake-goto-next-error)
  (my-flymake-show-error))

(defun my-flymake-goto-prev-error ()
  (interactive)
  (my-flymake-check-and-wait)
  (flymake-goto-prev-error)
  (my-flymake-show-error))

(defun py-find-file (errormark filename defaultdir)
  (let ((fullname (expand-file-name filename defaultdir)))
    (or (and (not (file-exists-p fullname))
             (let* ((name (loop for name = fullname then (directory-file-name
                                                          (file-name-directory name))
                                if (file-exists-p name) return name))
                    (fmt (and name (with-temp-buffer
                                     (insert-file-contents name nil 0 1024 t) (archive-find-type))))
                    (tail (and fmt (substring fullname (1+ (length name))))))
               (if fmt
                   (with-current-buffer (find-file-noselect name)
                     (goto-char (point-min))
                     (re-search-forward (concat " " (regexp-quote tail) "$"))
                     (save-window-excursion
                       (archive-extract)
                       (current-buffer))))))
        (compilation-find-file errormark filename defaultdir))))

(require 'arc-mode)

(defun py-eshell-goto-error (&optional back)
  (interactive "P")
  (display-buffer "*eshell*" t)
  (let (here end start dir file line errmk example)
    (with-current-buffer "*eshell*"
      (save-excursion
        ;; Find and validate last traceback
        (goto-char (point-max))
        (re-search-backward "^\\(.*\\)Traceback \\|^Failed example:")
        (beginning-of-line)
        (if (looking-at "Failed example:")
            (progn
              (forward-line -2)
              (setq example t)))
        (if (or (not (and (boundp 'py-eshell-last-error)
                          (boundp 'py-eshell-last-point)))
                (null py-eshell-last-error)
                (null py-eshell-last-point)
                (null py-eshell-last-dir)
                (not (= py-eshell-last-error (point))))
            (progn
              (set (make-local-variable 'py-eshell-last-error) (point))
              (set (make-local-variable 'py-eshell-prefix) (or (match-string 1) ""))
              (if (string-match "Original $" py-eshell-prefix)
                  (setq py-eshell-prefix (substring py-eshell-prefix 0 (match-beginning 0))))
              (if example
                  (forward-line 2)
                (while (and (< (forward-line 1) 1) (looking-at (concat py-eshell-prefix "  ")))))
              (set (make-local-variable 'py-eshell-last-point) (point))
              (set (make-local-variable 'py-eshell-last-dir) default-directory)
              (while
                  (and (< (forward-line 1) 1)
                       (not (if (looking-at ".*Leaving directory ")
                                (progn
                                  (goto-char (match-end 0))
                                  (skip-chars-forward "'\"`")
                                  (let ((dir (current-word)))
                                    (and dir (setq py-eshell-last-dir
                                                   (file-name-as-directory dir)))))))))))
        (goto-char py-eshell-last-point)

        ;; Nove to next / prev frame in traceback
        (if back
            (progn
              (while (and (looking-at (concat py-eshell-prefix "  "))
                          (< (forward-line 1) 1)
                          (not (looking-at (concat py-eshell-prefix "  File ")))))
              (setq start (point))
              (while (and (looking-at (concat py-eshell-prefix "  "))
                          (< (forward-line 1) 1)
                          (not (looking-at (concat py-eshell-prefix "  File ")))))
              (setq end (point)))
          (while (and (> (forward-line -1) -1)
                      (not (looking-at (concat py-eshell-prefix (if example "File " "  File "))))
                      (> (point) py-eshell-last-error)))
          (setq end py-eshell-last-point start (point)))

        ;; Parse information and set overlay
        (if (save-excursion (goto-char start) (setq errmk (point-marker))
                            (looking-at (concat py-eshell-prefix (if example "File " "  File "))))
            (let ((default-directory py-eshell-last-dir))
              (set (make-local-variable 'py-eshell-last-point) start)
              (if (and (boundp 'py-eshell-overlay) py-eshell-overlay)
                  (move-overlay py-eshell-overlay start end)
                (set (make-local-variable 'py-eshell-overlay) (make-overlay start end))
                (overlay-put py-eshell-overlay 'face 'highlight))
              (save-excursion
                (goto-char start)
                (forward-char (+ (length py-eshell-prefix) 7))
                (skip-chars-forward "\"")
                (setq file (current-word))
                (search-forward " line ")
                (skip-chars-forward " ")
                (setq line (string-to-number
                            (buffer-substring-no-properties
                             (point) (progn (skip-chars-forward "0-9") (point)))))
                (setq dir default-directory))
              (if (null file)
                  (error "File not found")))
          (py-eshell-error-reset)
          (error "No further traceback line"))))

    ;; move to error locus
    (if (and file line errmk)
        (with-current-buffer (py-find-file errmk file dir)
          (compilation-goto-locus errmk (save-excursion (goto-line line) (point-marker)) nil)))))

(defun py-eshell-error-reset ()
  (interactive)
  (save-excursion
    (set-buffer "*eshell*")
    (if (and (boundp 'py-eshell-overlay) py-eshell-overlay)
        (delete-overlay py-eshell-overlay))
    (set (make-local-variable 'py-eshell-last-error) nil)
    (set (make-local-variable 'py-eshell-last-point) nil)
    (set (make-local-variable 'py-eshell-last-dir) nil)
    (set (make-local-variable 'py-eshell-prefix) nil)
    (set (make-local-variable 'py-eshell-overlay) nil)))

(global-set-key "\C-xP" 'py-eshell-goto-error)
(global-set-key "\C-x\C-p" 'python-shell)

(add-hook 'python-mode-hook 'python-init-auto-cleanup-imports-on-save)

(defun py-setup-hook ()
  ;(set-variable 'py-indent-offset 4)
  ;(set-variable 'py-smart-indentation nil)
  (set-variable 'indent-tabs-mode nil)
  ;(define-key py-mode-map (kbd "RET") 'newline-and-indent)
  ;(define-key py-mode-map [tab] 'yas/expand)
  ;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
  ;(smart-operator-mode-on)
  (define-key python-mode-map "\C-ci" 'my-pyflymake-add-import-from-error)
  (define-key python-mode-map "\C-ce" 'my-flymake-show-error)
  (define-key python-mode-map "\C-cn" 'my-flymake-goto-next-error)
  (define-key python-mode-map "\C-cp" 'my-flymake-goto-prev-error)
  (define-key python-mode-map "\C-cI" 'py-cleanup-imports)
)

(add-hook 'python-mode-hook 'py-setup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if nil
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
  )
;(define-key py-mode-map "\t" 'ryan-python-tab)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Auto Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Syntax Error Hightlight

(defun my-python3-p ()
  (or (save-excursion
        (goto-char (point-min))
        (looking-at ".*python3"))
      (string-match-p " 3\." (shell-command-to-string "python --version"))))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((py3-p (my-python3-p))
           (temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (message "flymake init pyflakes %s" local-file)
      (list (if py3-p "pyflakes3" "pyflakes") (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(defun safer-flymake-find-file-hook ()
  "Don't barf if we can't open this flymake file"
  (let ((flymake-filename
         (flymake-create-temp-inplace (buffer-file-name) "flymake")))
    (if (file-writable-p flymake-filename)
        (flymake-find-file-hook)
      (message
       (format
        "Couldn't enable flymake; permission denied on %s" flymake-filename)))))
(add-hook 'find-file-hook 'safer-flymake-find-file-hook)

(defun py-skip-few-lines (&optional count)
  (if (null count) (setq count 3))
  (let ((blanks 0))
    (while
        (and (or (when (eolp) (setq count 0) (incf blanks) t)
                 (when (> count 0) (decf count) t))
             (< (forward-line 1) 1)))
    (> blanks 0)))

(defun py-imports-region ()
  (save-excursion
    (goto-char (point-min))
    (while (and (re-search-forward "^\\(import\\s-+\\|from\\s-+\\)" nil t)
                (looking-at "__future__")))
    (beginning-of-line)
    (setq beg (point))
    (while (and (py-skip-few-lines)
                (looking-at "import\\s-+\\|from\\s-+"))
      (setq beg (point)))
    (if (not (looking-at "\\(import\\s-+\\|from\\s-+\\)"))
        (cons beg beg)
      (while (looking-at "\\(import\\s-+\\|from\\s-+\\)")
        (forward-line 1))
      (cons beg (point)))))

(defun py-cleanup-imports (&optional nowait)
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq reg (py-imports-region))
      (if (= (car reg) (cdr reg))
          (error "No imports found"))
      (setq beg (car reg) end (cdr reg)))
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
            (my-flymake-check-and-wait)
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
