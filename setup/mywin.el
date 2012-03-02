(defun split-window-3-horizontally ()
  "Split window horizontally into three equal sized windows"
  (interactive)
  (let ((w (window-width)))
    (split-window-horizontally (- w (/ w 3) -2))
    (split-window-horizontally)))

(defun split-window-n-horizontally (width &optional min)
  "Split window horizontally into WIDTH wide windows making the last
window no smaller than MIN. MIN defaults to WIDTH and WIDTH defaults
to 80."
  (interactive "P")
  (if (not width) (setq width 80))
  (if (not min) (setq min width))
  (save-selected-window
    (while (> (window-width) (+ width 3 min 3))
      (select-window (split-window-horizontally (+ width 3)))
      (switch-to-buffer (get-buffer-create "*scratch*")))))

(defun maximize-window(&optional min-height)
  "Enlarge the current window by as many lines as is possible without making any other
window smaller than MIN-HEIGHT lines."
  (interactive)
  ;; this algorithm is copied from window.el / balance-windows()
  (let ((min-height (or min-height i(+ window-min-height 0)))
        (count -1)
        size)
        ;; Don't count the lines that are above the uppermost windows.
        ;; (These are the menu bar lines, if any.)
    ;; Find all the different vpos's at which windows start,
    ;; then count them.  But ignore levels that differ by only 1.
    (save-window-excursion
      (let (tops (prev-top -2))
        (walk-windows (function (lambda (w)
                                  (setq tops (cons (nth 1 (window-edges w))
                                                   tops))))
                      'nomini)
        (setq tops (sort tops '<))
        (while tops
          (if (> (car tops) (1+ prev-top))
              (setq prev-top (car tops)
                    count (1+ count)))
          (setq tops (cdr tops)))
        (setq count (1+ count))))
    (setq size (- (frame-height) (* (1- count) min-height)))
    (enlarge-window (- size (window-height)))))

(defun safe-shrink-window(&optional n min-height)
  "Like shrink-window but will not remove a window"
  (interactive)
  (let* ((min-height (or min-height window-min-height))
         (n (or n 1)))
    (if (< (- (window-height) n) min-height)
        (shrink-window (- (window-height) min-height))
      (shrink-window n))))

(defun setup-my-windows ()
  (interactive)
  (let ((width 100) (min 100) (distribute t)
        (currentwindow (selected-window)) topwindows  newwindow newtopwindows)
    (walk-windows (function (lambda (w)
                              (let ((e (window-edges w)))
                                (if (< (nth 1 e) window-min-height)
                                    (setq topwindows (cons (list (nth 0 e)
                                                                 w
                                                                 (window-buffer w)
                                                                 (window-point w)
                                                                 (window-start w)
                                                                 (equal w currentwindow))
                                                           topwindows))))))
                            'nomini)
    (setq topwindows (sort topwindows (function (lambda (a b) (< (car a) (car b))))))
    (delete-other-windows (nth 1 (car topwindows)))
    (setq topwindows (cdr topwindows))
    (save-selected-window
      (select-window (split-window-vertically
                      (- (window-height) (max 5 (/ (* (frame-height) 15) 100)) -1)))
      (switch-to-buffer (get-buffer-create "*compilation*")))
    (setq newwindow (selected-window))
    (setq newtopwindows (list (selected-window)))
    (while (> (window-width) (+ width 3 min 3))
      (select-window (split-window-horizontally (+ width 3)))
      (setq newtopwindows (cons (selected-window) newtopwindows))
      (if topwindows
          (progn
            (switch-to-buffer (nth 2 (car topwindows)))
            (set-window-start (selected-window) (nth 4 (car topwindows)))
            (goto-char (nth 3 (car topwindows)))
            (if (nth 5 (car topwindows))
                (setq newwindow (selected-window)))
            (setq topwindows (cdr topwindows)))
        (switch-to-buffer (get-buffer-create "*scratch*"))))
    (select-window newwindow)
    (if (and distribute (> (length newtopwindows) 1))
        (balance-windows newwindow))))

(defun my-split-window-sensibly (window)
  (if (and (> (window-height window) (- (frame-height (window-frame window)) window-min-height))
           (> (window-height window) (max 5 (/ (* (frame-height) 15) 100))))
      (split-window-sensibly window)))

(setq split-window-preferred-function 'my-split-window-sensibly)

(global-set-key "\C-x7" 'split-window-3-horizontally)
(global-set-key "\C-x8" (lambda () (interactive) (split-window-n-horizontally 100 50)))
(global-set-key "\C-x9" 'setup-my-windows)
(global-set-key "\C-xp" 'other-window-reverse)
(global-set-key "\C-\M-_" (lambda () (interactive) (safe-shrink-window 5)))

(defun maximize-window-15 ()
  (interactive)
  (maximize-window (max 5 (/ (* (frame-height) 15) 100))))

(global-set-key [(ctrl meta ?+)]  'maximize-window-15)
