(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "magit/lisp"))

(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "with-editor"))

(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "auto-install"))

(require 'cl)

(if (not (functionp 'run-hook-wrapped))
    (defun run-hook-wrapped (hook wrap-function &rest args)
      (loop for fn in hook
            thereis (apply 'wrap-function fn args))))

(if (not (functionp 'process-live-p))
    (defun process-live-p (process)
      (memq (process-status process)
            '(run open listen connect stop))))

(require 'magit)

(setq magit-refs-local-branch-format "%C %-48n %U%m\n")
(setq magit-refs-remote-branch-format "%C %-48n %m\n")

(when (eq system-type 'windows-nt)

  (require 'advice)

  (setf (symbol-function 'builtin-process-file) (symbol-function 'process-file))

  (defvar my-magit-shell "c:\\Program Files (x86)\\Git\\bin\\sh")

  (defun my-magit-process-file (program &optional infile buffer display &rest args)
    (builtin-process-file my-magit-shell infile buffer display
                          "-c" (mapconcat 'shell-quote-argument (cons "/bin/git" args) " ")))

  (defadvice magit-cmd-output (around my-magit-process-file activate)
    (letf (((symbol-function 'process-file) (symbol-function 'my-magit-process-file)))
      ad-do-it))

  (defadvice magit-git-exit-code (around my-magit-process-file activate)
    (letf (((symbol-function 'process-file) (symbol-function 'my-magit-process-file)))
      ad-do-it))

  (defadvice magit-run (around activate)
    (letf (((symbol-function 'process-file) (symbol-function 'my-magit-process-file)))
      ad-do-it))

); End Windows-NT

(defadvice magit-mode-quit-window (around my-magit-mode-quit-window activate)
  (letf (((symbol-function 'selected-window) (lambda ())))
    ad-do-it))

(global-set-key "\C-cGS" 'magit-status)

(defun my-shell-command-to-string (cmd)
  (shell-command cmd " *my-shell-command-to-string*")
  (save-current-buffer
    (set-buffer " *my-shell-command-to-string*")
    (prog1
        (buffer-string)
      (kill-buffer " *my-shell-command-to-string*"))))

(defun git-repo-files ()
  (let ((default-directory (magit-toplevel default-directory)))
    (split-string (my-shell-command-to-string "git ls-files") "\n")))

(defun find-file-in-git-repo ()
  (interactive)
  (let ((repo (magit-toplevel default-directory))
        (files (git-repo-files)))
    (find-file
     (concat repo
             (ido-completing-read
              "Find file in git repo: "
              (remove-if (lambda (x) (string= "" x))
              files))))))

(defun grep-in-git-repo (regexp &optional words-only)
  (interactive "sGrep files in Git repo regexp: \nP")
  (let ((default-directory (magit-toplevel default-directory)))
    (if (not default-directory)
        (error "not a Git directory"))
    (grep (format "git ls-files -z | xargs -r0 grep -d skip -nH -E%s -- %s"
                  (if words-only " -w" "") (shell-quote-argument regexp)))))

(setenv "GIT_PAGER" "cat")
(setenv "GIT_MAN_VIEWER" "woman")
(setenv "GIT_EDITOR" "emacsclient")

(defun find-file-maybe-git (&optional nogit)
  (interactive "P")
  (if (and (not nogit) (magit-toplevel default-directory))
      (call-interactively 'find-file-in-git-repo)
    (call-interactively 'ido-find-file)))

(global-set-key "\C-x\C-f" 'find-file-maybe-git)
(global-set-key "\C-cGG" 'grep-in-git-repo)

(defun git-files-find-symbol (symbol)
  (interactive (list (read-string "Symbol: " (current-word))))
  (let ((dir (magit-toplevel default-directory)))
    (if (not dir) (error "No git repository"))
    (let ((default-directory dir))
      (grep (format "git ls-files -z | xargs -r0 grep -d skip -nwHF %s" symbol)))))

(defun git-files-find-class-decl (symbol)
  (interactive (list (read-string "Symbol: " (current-word))))
  (let ((dir (magit-toplevel default-directory)))
    (if (not dir) (error "No git repository"))
    (let ((default-directory dir))
      (grep (format (concat "git ls-files -z"
                            " | xargs -r0 grep -d skip -nwHF %s"
                            " | grep -Ew '(class|struct|typedef|using)'"
                            " | grep -vEw 'friend'")
                    symbol)))))

(global-set-key "\C-cGF" 'git-files-find-symbol)
(global-set-key "\C-cGC" 'git-files-find-class-decl)

(defun dired-git-files ()
  (interactive)
  (let ((default-directory (magit-toplevel default-directory))\
        (ls-lisp-use-insert-directory-program t)
        files)
    (setq files (delete-if '(lambda (file) (string= file ""))
                           (split-string (shell-command-to-string "git ls-files") "\n")))
    (dired (cons default-directory files))))

(global-set-key "\C-cGD" 'dired-git-files)

(defun dired-grep-git-files (regexp &optional words-only)
  (interactive "sRegexp: \nP")
  (let ((default-directory (magit-toplevel default-directory))
        (cmd (format "git ls-files -z | xargs -r0 grep -d skip -l -E%s -- %s"
                     (if words-only " -w" "") (shell-quote-argument regexp))))
    (if (not default-directory)
        (error "not in Git repository"))
    (setq files (delete-if '(lambda (file) (string= file ""))
                           (split-string (shell-command-to-string cmd)  "\n")))
    (dired (cons default-directory files))))

(global-set-key "\C-cGH" 'dired-grep-git-files)

(defun magit-svn-fetch ()
  (interactive)
  (magit-run-git-async "svn" "fetch"))

(define-key magit-mode-map "Nf" 'magit-svn-fetch)

(defun magit-quit-window (&optional kill-buffer)
  (interactive "P")
  (quit-window kill-buffer))

(defun magit-diff-master-mergebase (&optional args files)
  (interactive (magit-diff-arguments))
  (magit-diff-working-tree
   (magit-git-string "merge-base" "master" "HEAD") args files))

(magit-define-popup-action 'magit-diff-popup
  ?m "Diff merge-base master" 'magit-diff-master-mergebase)

(magit-define-popup-switch 'magit-log-popup
  ?f "first parent" "--first-parent")

(require 'ffap)

(defun g0dil-magit-old-version-jump-to-current ()
  (interactive)
  (let ((current-file (ffap-file-exists-string
                       (file-name-nondirectory
                        (replace-regexp-in-string "\\.~.*$" "" (buffer-name))))))
    (if current-file
        (g0dil-goto-equivalent-position current-file)
      (error "current version of file not found"))))

(define-key magit-blob-mode-map "c" 'g0dil-magit-old-version-jump-to-current)

(defun g0dil-magit-diff-jump-to-current ()
  (interactive)
  (let ((section-file-name (loop for ident in (magit-section-ident (magit-current-section))
                                 if (and (consp ident) (eq (car ident) 'file))
                                 return (cdr ident)
                                 finally return nil)))
    (if (ffap-file-exists-string section-file-name)
        (g0dil-goto-equivalent-position section-file-name)
      (error "current version of file not found"))))

(define-key magit-revision-mode-map (kbd "C-c RET") 'g0dil-magit-diff-jump-to-current)

; ignore whitespace
; (setq magit-diff-options '("-w"))
