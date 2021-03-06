;;; cython-mode.el -- A derivation of python-mode for Cython .pyx and .pxd files

(require 'python-mode)
(define-derived-mode cython-mode python-mode "Cython"
  (font-lock-add-keywords
   nil
   `((,(concat "\\<\\(NULL"
               "\\|c\\(def\\|har\\|typedef\\)"
               "\\|e\\(num\\|xtern\\)"
               "\\|float"
               "\\|in\\(clude\\|t\\)"
               "\\|object\\|public\\|struct\\|type\\|union\\|void"
               "\\|cppclass\\|cimport\\|namespace\\|inline"
               "\\)\\>")
      1 font-lock-keyword-face t))))
(provide 'cython-mode)

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
