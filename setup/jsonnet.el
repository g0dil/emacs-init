(add-to-list 'load-path (concat (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name
                                       (when (boundp 'bytecomp-filename) bytecomp-filename)
                                       buffer-file-name))))
                                "jsonnet-mode"))

(require 'jsonnet-mode)

(defun my-jsonnet-insert-matching-parens (paren)
  (interactive "cKind of matching parens to insert? ('{', '[', '(')")
  (indent-region
   (prog1
       (point)
     (insert paren))
   (save-excursion
     (backward-up-list)
     (forward-sexp)
     (forward-char -1)
     (insert (cdr (assq paren '((?( . ?)) (?[ . ?]) (?{ . ?})))) "\n")
     (forward-char 2)
     (point))))

(defun my-jsonnet-wrap-with-matching-parens (paren)
  (interactive "cKind of matching parens to insert? ('{', '[', '(')")
  (insert paren)
  (save-excursion
    (forward-sexp)
    (insert (cdr (assq paren '((?( . ?)) (?[ . ?]) (?{ . ?})))) "\n")))

(defun my-jsonnet-add-comma ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (forward-sexp)
    (insert ",")))

(define-key jsonnet-mode-map "\C-c[" 'my-jsonnet-insert-matching-parens)
(define-key jsonnet-mode-map "\C-c]" 'my-jsonnet-wrap-with-matching-parens)
(define-key jsonnet-mode-map "\C-c," 'my-jsonnet-add-comma)

(require 'cc-defs)

(defconst my-jsonnet-syntax-table
  (let ((table (make-syntax-table jsonnet-mode-syntax-table)))
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+ "." table)
    table))

(defun my-parse-jsonnet-name ()
  (save-excursion
    (c-skip-ws-forward)
    (if (looking-at "['\"]")
      (substring (thing-at-point 'sexp t) 1 -1)
      (with-syntax-table my-jsonnet-syntax-table
        (current-word)))))

(defun my-parse-jsonnet-fn ()
  (save-excursion
    (let ((fn (my-parse-jsonnet-name)))
      (search-forward fn nil 'move)
      (c-skip-ws-forward)
      (if (not (looking-at "("))
        (error "invalid function: paranthesis not found"))
      (cons fn (my-parse-jsonnet-args)))))

(defun my-parse-jsonnet-args ()
  (with-syntax-table my-jsonnet-syntax-table
    (save-excursion
      (loop
         do (c-skip-ws-forward)
         while (looking-at "[(,]")
         do (progn (forward-char 1) (c-skip-ws-forward))
         while (not (looking-at ")"))
         for arg = (current-word)
         do (progn (search-forward arg nil 'move) (c-skip-ws-forward))
         if (looking-at "=")
           do (progn (forward-char 1) (c-skip-ws-forward))
           and for dfl =(let ((db (point)))
                          (loop
                             do (forward-sexp)
                             do (c-skip-ws-forward)
                             while (not (looking-at "[,)]")))
                          (buffer-substring-no-properties db (point)))
         else
           for dfl = nil
         collect (cons arg dfl)))))

(defun my-docsonnet-fn ()
  (interactive)
  (let ((fn (my-parse-jsonnet-fn)))
    (save-excursion
      (insert "'#" (car fn) "':: __.fn(\n|||\n  " (car fn) "\n|||,\n")
      (if (cdr fn)
        (loop initially (insert "[")
              for (arg . dfl) in (cdr fn)
              do (insert "__.arg('" arg "', __.T.any")
              if dfl do (insert ", default=" (if (string= dfl "null") "__.T.nil" dfl))
              do (insert "),\n")
              finally (insert "],\n")))
      (insert "),"))
    (jsonnet-reformat-buffer)))

(defun my-docsonnet-import ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (not (search-forward "local __ = import" nil t))
      (insert "/* used only when rendering the documentation */ "
              "local __ = import 'doc-util/main.libsonnet';\n\n"))))

(defun my-docsonnet-pkg ()
  (interactive)
  (save-excursion
    (insert "'#':: __.pkg(\n"
            "name='" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) "',\n"
            "url='" (file-name-nondirectory (buffer-file-name)) "',\n"
            "help='" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) "',\n"
            "),"))
  (jsonnet-reformat-buffer))

(defun my-docsonnet-val ()
  (interactive)
  (let ((val (my-parse-jsonnet-name)))
    (save-excursion
      (insert "'#" val "':: __.val(\n|||\n  " val "\n|||,\n__.T.any,\nself." val "),"))
    (jsonnet-reformat-buffer)))

(defun my-docsonnet-obj ()
  (interactive)
  (let ((val (my-parse-jsonnet-name)))
    (save-excursion
      (insert "'#" val "':: __.obj(\n|||\n  " val "\n|||,\n),"))
    (jsonnet-reformat-buffer)))

(define-key jsonnet-mode-map "\C-cdi" 'my-docsonnet-import)
(define-key jsonnet-mode-map "\C-cdp" 'my-docsonnet-pkg)
(define-key jsonnet-mode-map "\C-cdf" 'my-docsonnet-fn)
(define-key jsonnet-mode-map "\C-cdv" 'my-docsonnet-val)
(define-key jsonnet-mode-map "\C-cdo" 'my-docsonnet-obj)

(defun my-jsonnet-from-hcl ()
  (interactive)
  (insert "{\n")
  (forward-sexp)
  (insert ": {\n")
  (forward-sexp)
  (insert ": {\n")
  (forward-sexp)
  (insert ":")
  (let* ((b (point))
         (e (progn (forward-sexp) (point))))
    (insert "}}},")
    (replace-string "=" ":" nil b e)
    (replace-regexp "[^,\n]$" "\\&," nil
                    (save-excursion
                      (goto-char b)
                      (skip-syntax-forward "^w")
                      (beginning-of-line)
                      (point))
                    (save-excursion
                      (goto-char e)
                      (beginning-of-line)
                      (forward-char -1)
                      (point))))
  (jsonnet-reformat-buffer))

(defun my-jsonnet-hcl-ref-at-point ()
  (interactive)
  (insert "'${")
  (end-of-line)
  (skip-chars-backward ",")
  (skip-syntax-backward "-")
  (insert "}'"))

(define-key jsonnet-mode-map "\C-chh" 'my-jsonnet-from-hcl)
(define-key jsonnet-mode-map "\C-chr" 'my-jsonnet-hcl-ref-at-point)
