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
				   (loop for att in xmltok-attributes
						 if (string= (xmltok-attribute-local-name att) "id")
						 return (values t (concat "\"" (xmltok-attribute-value att) "\""))
						 finally return (values nil (xmltok-start-tag-local-name)))
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
