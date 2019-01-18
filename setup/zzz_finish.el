(load custom-file)

; Don't ask why we need this ...

; I just don't know where why this is enabled by default in my config ... can't find it
(setq debug-on-error nil)

; This load consistently fails the first time round
(load-library "python")

; 'lines-tail' is valid but missing from customize :-/
(setq whitespace-style
      (nconc (delete 'lines (delete 'lines-tail whitespace-style))
             '(lines-tail)))

; For some unfathomable reason display-buffer always want's to resize my windows ...
(defadvice display-buffer-use-some-window (around no-resize activate)
  (flet ((window-resize (&rest args) nil))
    ad-do-it))

; After everybody has had a chance to mess with flymake, remove the flymake modes which just don't work
(setq flymake-allowed-file-name-masks
      (loop for elt in flymake-allowed-file-name-masks
            if (not (member (car elt) '("\\.xml\\'" "\\.html?\\'" "\\.cs\\'" "\\.p[ml]\\'"
                                        "\\.h\\'" "\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'")))
            collect elt))
