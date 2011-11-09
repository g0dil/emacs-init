(require 'json)
(require 'url-http)

(defvar dpservice-dbspec-url "http://dpservice.traveltainment.int/dbspec")
(defvar dpservice-dbspec)

(defun dpservice-dbspec-loaded ()
  (let ((json-array-type 'list)
		(json-false nil))
	(message "Parsing ...")
	(forward-paragraph)
	(setq dpservice-dbspec (json-read))
	(message "Done.")
	(kill-buffer (current-buffer))))

(defun dpservice-load-dbspec ()
  (interactive)
  (let ((url-http-version "1.0")) ; no chunked response
	(url-http (url-generic-parse-url dpservice-dbspec-url) 'dpservice-dbspec-loaded '())))

(defun dpservice-maybe-load-dbspec ()
  (if (not dpservice-dbspec)
	  (progn
		(dpservice-load-dbspec)
		(while (not dpservice-dbspec)
		  (sit-for 1)))))

(defun dpservice-make-csv-header (type table separator)
  (loop for lastn = 1 then n
		for (n . name) in (sort (loop for spalte in (cdr (assq 'spalten 
															   (cdr (assq table
																		  (cdr (assq type dpservice-dbspec))))))
									  for n = (cdr (assq 'csvWert spalte))
									  if (numberp n)
									  collect (cons n (cdr (assq 'name spalte))))
								'(lambda (a b) (< (car a) (car b))))
		concat (make-string (- n lastn) separator)
		concat name))

(defun dpservice-types ()
  (mapcar 'car dpservice-dbspec))

(defun dpservice-tables (type)
  (mapcar 'car (cdr (assq type dpservice-dbspec))))

(defun dpservice-csv-insert-header (type table &optional separator)
  (interactive 
   (progn
	 (dpservice-maybe-load-dbspec)
	 (let* ((type (intern (completing-read "Type: " (mapcar 'symbol-name (dpservice-types)))))
			(table (intern (completing-read "Table: " (mapcar 'symbol-name (dpservice-tables type))))))
	   (list type table))))
  (goto-char (point-min))
  (insert (dpservice-make-csv-header type table (or separator (car csv-separator-chars))) "\n")
  (csv-align-fields nil (point-min) (point-max)))
