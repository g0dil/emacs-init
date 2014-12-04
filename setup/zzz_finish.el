; Don't ask why we need this ...

; I just don't know where why this is enabled by default in my config ... can't find it
(setq debug-on-error nil)

; This load consistently fails the first time round
(load-library "python")

; 'lines-tail' is valid but missing from customize :-/
(setq whitespace-style
      (nconc (delete 'lines (delete 'lines-tail whitespace-style))
             '(lines-tail)))
