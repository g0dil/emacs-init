(defvar nxml-where-elements-to-id 3)
(defvar nxml-where-max-elements 6)

(require 'cl)

(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (and (eq major-mode 'nxml-mode)
	   (let (path-to-id path-rest)
		 (save-excursion
		   (save-restriction
			 (widen)
			 (while (and (not (bobp))
						 (condition-case nil (progn (nxml-backward-up-element) t) (error nil)))
			   (multiple-value-bind 
				   (has-id step)
				   (loop with has-id = nil
                         with step = (xmltok-start-tag-local-name)
					     for att in xmltok-attributes
						 if (string= (xmltok-attribute-local-name att) "id")
						 return (values t (concat "\"" (xmltok-attribute-value att) "\""))
						 else if (string= (xmltok-attribute-local-name att) "name")
						 do (setq has-id t step (concat "\"" (xmltok-attribute-value att) "\""))
						 finally return (values has-id step ))
				 (if (or path-to-id has-id)
					 (setq path-to-id (cons step path-to-id))
				   (setq path-rest (cons step path-rest)))))))
		 (let ((path-to-id-len (length path-to-id))
			   (path-rest-len (length path-rest)))
		   (if (> path-to-id-len nxml-where-elements-to-id)
			   (progn
				 (setq path-to-id (nthcdr (- path-to-id-len nxml-where-elements-to-id -1) path-to-id))
				 (setq path-to-id (cons "..." path-to-id))
				 (setq path-to-id-len nxml-where-elements-to-id))
			 (setq path-to-id (cons "" path-to-id)))
		   (when (> (+ path-to-id-len path-rest-len) nxml-where-max-elements)
			 (setq path-rest (nbutlast path-rest (- path-rest-len (- nxml-where-max-elements path-to-id-len) -1)))
			 (setq path-rest (nconc path-rest (list "...")))))
		 (mapconcat 'identity (nconc path-to-id path-rest) "/"))))
  
(require 'which-func)

(add-to-list 'which-func-functions 'nxml-where)
(add-to-list 'which-func-modes 'nxml-mode)
(add-to-list 'which-func-non-auto-modes 'nxml-mode)

(add-to-list 'auto-mode-alist '("\\.xslt?\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))

(require 'hideshow)

(add-to-list 'hs-special-modes-alist '(nxml-mode ("\\(<[^/>]*>\\)$" 1)
												 "</[^/>]*>$"))
(defun nxml-enable-hs ()
  (setq nxml-sexp-element-flag t)
  (hs-minor-mode 1))

(add-hook 'nxml-mode-hook 'nxml-enable-hs)

(defun hs-nxml-enter ()
  (interactive)
  (when (hs-already-hidden-p)
	(hs-show-block)
	(hs-hide-level 1)
	(nxml-forward-element)
	(nxml-backward-element)))

(defun hs-nxml-leave ()
  (interactive)
  (nxml-backward-up-element)
  (hs-hide-block)
  (nxml-backward-up-element))

(defun hs-nxml-hide-other ()
  (interactive)
  (let ((p (point)))
    (hs-hide-all)
    (while (progn (goto-char p) (hs-already-hidden-p))
      (hs-nxml-enter))
    (hs-show-block)
    (goto-char p)
    (recenter-top-bottom)))

(define-key nxml-mode-map (kbd "\C-c <left>") 'hs-nxml-leave)
(define-key nxml-mode-map (kbd "\C-c <right>") 'hs-nxml-enter)
(define-key nxml-mode-map (kbd "\C-c @ o") 'hs-nxml-hide-other)

(defun nxml-complete-and-autoclose-element (use-region)
  (interactive "P")
  (let* ((start (if use-region (setq start (set-marker (make-marker) (region-beginning)))))
         (end (setq use-region (set-marker (make-marker) (region-end))))
         (beg (and start (= (point) start))))
  (save-excursion
    (insert " "))
  (nxml-complete)
  (let ((name (xmltok-start-tag-local-name)))
    (delete-char 1)
    (if (and start end)
        (progn
          (if (not beg)
              (progn
                (delete-char (- (1+ (length name))))
                (goto-char start)
                (insert "<" name ">"))
            (insert ">"))
          (goto-char end))
      (insert ">"))
    (save-excursion
      (insert "</" name ">")))))

(define-key nxml-mode-map (kbd "\C-c ." ) 'nxml-complete-and-autoclose-element)
(define-key nxml-mode-map (kbd "\C-c\C-c") 'recompile)

(defconst nxml-docbook-common-elements
  '(("section" . ("para" "itemizedlist" "variablelist" "section" "bridgehead" "task" "procedure"))
    ("para" . ("emphasis" "code" "replaceable"))
    ("emphasis" . ("code"))
    ("itemizedlist" . ("listitem"))
    ("orderedlist" . ("listitem"))
    ("variablelist" . ("varlistentry"))
    ("varlistentry" . ("term" "listitem"))
    ("term" . ("emphasis" "code"))
    ("listitem" . ("para" "itemizedlist"))
    ("task" . ("tasksummary" "procedure"))
    ("tasksummary" . ("para"))
    ("procedure" . ("step"))
    ("step" . ("para" "procedure"))
    ("mathphrase" . ("replaceable" "superscript" "subscript"))
    ("title" . ("code" "replaceable"))
    ("literallayout" . ("replaceable" "emphasis" "code"))))

(defvar nxml-docbook-last-common-element nil)

(defun nxml-docbook-make-common-element ()
  (interactive)
  (let ((start (set-marker (make-marker) (point)))
        (end (set-marker (make-marker) (point)))
        do-region)
    (when (or (region-active-p)
              (and (eq real-last-command 'nxml-docbook-make-common-element)
                   (car nxml-docbook-last-common-element)))
      (set-marker start (region-beginning))
      (set-marker end (region-end))
      (message "do-mark %s %s" start end)
      (setq do-region t))
    (message "cycle? %s %s" real-last-command nxml-docbook-last-common-element)
    (when (and (eq real-last-command 'nxml-docbook-make-common-element)
               (cdr nxml-docbook-last-common-element))
      (delete-region (save-excursion (skip-chars-backward "^<") (1- (point))) start)
      (delete-region end (save-excursion (skip-chars-forward "^>") (1+ (point)))))
    (let* ((token-end (nxml-token-before)) 
           (start-tag-end
            (save-excursion
              (when (and (< (point) token-end)
                         (memq xmltok-type
                               '(cdata-section
                                 processing-instruction
                                 comment
                                 start-tag
                                 end-tag
                                 empty-element)))
                (setq nxml-docbook-last-common-element nil)
                (error "Point is inside a %s"
                       (nxml-token-type-friendly-name xmltok-type)))
              (nxml-scan-element-backward token-end t)))
           (context (xmltok-start-tag-qname))
           (elements (cdr (assoc context nxml-docbook-common-elements)))
           (index (if (and elements
                           (eq real-last-command 'nxml-docbook-make-common-element)
                           (cdr nxml-docbook-last-common-element))
                      (1+ (cdr nxml-docbook-last-common-element))
                    0))
           (element (and elements (nth index elements))))
      (when (not elements)
        (setq nxml-docbook-last-common-element nil)
        (error "No common elements for %s" context))
      (if element
          (progn
            (goto-char start)
            (insert-before-markers "<" element ">")
            (goto-char end)
            (insert "</" element ">")
            (goto-char end)
            (setq nxml-docbook-last-common-element (cons do-region index)))
        (setq nxml-docbook-last-common-element (cons do-region nil)))
      (when do-region
        (set-mark start)
        (message  "Fiddlesticks: %s %s %s" (mark t) mark-active (region-active-p))))))

(defun nxml-just-one-space-or-skip-end ()
  (interactive)
  (if (looking-at "</")
      (progn
        (skip-chars-forward "^>")
        (forward-char 1))
    (just-one-space)))

(define-key nxml-mode-map (kbd "M-RET") 'nxml-docbook-make-common-element)
(define-key nxml-mode-map (kbd "M-SPC") 'nxml-just-one-space-or-skip-end)

(defun nxml-open-line ()
  (interactive)
  (open-line 1)
  (save-excursion
    (forward-line 1)
    (indent-according-to-mode))
  (newline-and-indent))

(define-key nxml-mode-map (kbd "M-o") 'nxml-open-line)
