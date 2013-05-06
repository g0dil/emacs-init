(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "emacs-egrep"))
(require 'magit)
(require 'cl)

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

(global-set-key "\C-cGS" 'magit-status)

(defun find-file-in-git-repo ()
  (interactive)
  (let* ((repo (magit-get-top-dir default-directory))
         (files (shell-command-to-string (format "cd %s && git ls-files" repo))))
    (find-file
     (concat repo
             (ido-completing-read
              "Find file in git repo: "
              (remove-if (lambda (x) (string= "" x))
              (split-string files "\n")))))))

(defun grep-in-git-repo (regexp)
  (interactive "sGrep files in Git repo regexp: ")
  (let ((default-directory (magit-get-top-dir default-directory)))
    (if (not default-directory)
        (error "not a Git directory"))
    (grep (format "git ls-files -z | xargs -r0 grep -nH -E %s | cat -"
                  (shell-quote-argument regexp)))))

(setenv "GIT_PAGER" "cat")
(setenv "GIT_MAN_VIEWER" "woman")
(setenv "GIT_EDITOR" "emacsclient")

(defun find-file-maybe-git (&optional nogit)
  (interactive "P")
  (if (and (not nogit) (magit-get-top-dir default-directory))
      (call-interactively 'find-file-in-git-repo)
    (call-interactively 'ido-find-file)))

(global-set-key "\C-x\C-f" 'find-file-maybe-git)
(global-set-key "\C-cGG" 'grep-in-git-repo)

(defun git-files-find-symbol (symbol)
  (interactive (list (read-string "Symbol: " (current-word))))
  (let ((dir (magit-get-top-dir default-directory)))
    (if (not dir) (error "No git repository"))
    (let ((default-directory dir))
      (grep (format "git ls-files -z | xargs -r0 grep -nwHF %s | cat -" symbol)))))

(global-set-key "\C-cGF" 'git-files-find-symbol)

(defun dired-git-files ()
  (interactive)
  (let ((default-directory (magit-get-top-dir default-directory))
        (ls-lisp-use-insert-directory-program t))
    (dired (cons default-directory (split-string (shell-command-to-string "git ls-files") "\n")))))

(global-set-key "\C-cGD" 'dired-git-files)
