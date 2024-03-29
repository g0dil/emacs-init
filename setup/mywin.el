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
  (let ((min-height (or min-height (+ window-min-height 0)))
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

(defconst setup-my-windows-precious-buffers
  '("*eshell*"))
(defconst setup-my-windows-junk-buffers
  '("*scratch*" "*Messages*" "*Calculator" "*Calc Trail*" "*compilation*" "*fetchmail*"))

(defvar my-windows-count nil)

(defun get-top-windows ()
  (let (topwindows)
    (walk-windows (function (lambda (w)
                              (let ((e (window-edges w)))
                                (if (< (nth 1 e) window-min-height)
                                  (setq topwindows (cons (cons (nth 0 e) w) topwindows)))))))
    (loop for w in (sort topwindows (function (lambda (a b) (< (car a) (car b)))))
          collect (cdr w) )))

(defun setup-my-windows (&optional n)
  (interactive "P")
  (if n
      (if (integerp n)
          (setq my-windows-count n)
        (setq my-windows-count nil)))
  (let* ((width (if my-windows-count
                    (- (/ (frame-width) my-windows-count) 4)
                  (or whitespace-line-column 100)))
         (min width) (distribute t)
         (currentbuffer (current-buffer))
         (currentwindow (selected-window))
         (topwindows (loop for w in (get-top-windows)
                           for b = (window-buffer w)
                           if (not (member (buffer-name b)
                                           setup-my-windows-junk-buffers))
                           collect (list (nth 0 (window-edges w))
                                         w
                                         b
                                         (window-point w)
                                         (window-start w)
                                         (equal w currentwindow))))
         firstwindow newwindow newtopwindows newbottomwindow)
    (delete-other-windows (nth 1 (car topwindows)))
    (save-selected-window
      (setq newbottomwindow (split-window-vertically
                             (- (window-height) (max 5 (/ (* (frame-height) 15) 100)) -1)))
      (select-window newbottomwindow)
      (switch-to-buffer (get-buffer-create "*compilation*"))
      (if (eq currentbuffer (current-buffer))
          (setq newwindow (selected-window))))
    (setq firstwindow (selected-window))
    (setq newtopwindows (list (selected-window)))
    (while (> (window-width) (+ width 3 min 3))
      (select-window (split-window-horizontally (+ width 3)))
      (setq newtopwindows (cons (selected-window) newtopwindows))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (setq newtopwindows (reverse newtopwindows))
    (loop for w in newtopwindows
          for (pos win buf point start iscurrent) in
              (loop for w in topwindows
                    for (pos win buf point start iscurrent) = w
                    if (not (member (buffer-name buf) setup-my-windows-precious-buffers))
                    collect w)
          do (progn
               (select-window w)
               (set-window-buffer w buf)
               (set-window-start w start)
               (goto-char point)
               (if iscurrent
                   (setq newwindow w))))
    (setq newtopwindows (reverse newtopwindows))
    (setq topwindows (reverse topwindows))
    (loop for w in newtopwindows
          for (pos win buf point start iscurrent) in
              (loop for w in topwindows
                    for (pos win buf point start iscurrent) = w
                    if (member (buffer-name buf) setup-my-windows-precious-buffers)
                    collect w)
          do (progn
               (select-window w)
               (set-window-buffer w buf)
               (set-window-start w start)
               (goto-char point)
               (if iscurrent
                   (setq newwindow w))))
    (setq newwindow
          (or newwindow
              (loop for w in newtopwindows
                    if (eq (window-buffer w) currentbuffer) return w)
              (loop for w in newtopwindows
                    for name = (buffer-name (window-buffer w))
                    if (string= name "*scratch*") return w)
              (loop for w in newtopwindows
                    for name = (buffer-name (window-buffer w))
                    if (and (= (aref name 0) ?*)
                            (not (member name setup-my-windows-precious-buffers))) return w)
              firstwindow))
    (when (and distribute (> (length newtopwindows) 1))
      (pjb-balance-windows t))
    (select-window newwindow)
    (if (not (member (buffer-name currentbuffer) setup-my-windows-junk-buffers))
        (switch-to-buffer currentbuffer))
    newbottomwindow))

(defun my-split-window-sensibly (window)
  (if (and (> (window-height window) (- (frame-height (window-frame window)) window-min-height))
           (> (window-height window) (max 5 (/ (* (frame-height) 15) 100))))
      (split-window-sensibly window)))

(defun my-pop-to-buffer (buffer)
  ;; display buffer in rightmost window if not displayed currently
  (let ((w (get-buffer-window buffer)))
    (unless w
      (setq w (car (last (get-top-windows)))))
    (select-window w)
    (switch-to-buffer buffer)))

(defun my-display-at-bottom (&optional buffer)
  ;; call my-setup-window and display current-buffer or BUFFER in bottom frame
  (interactive)
  (if (not buffer) (setq buffer (current-buffer)))
  (bury-buffer)
  ;; why does save-selected-window not work here ???
  (save-selected-window
    (select-window (setup-my-windows))
    (switch-to-buffer buffer)))

(setq split-window-preferred-function 'my-split-window-sensibly)

(global-set-key "\C-x7" 'split-window-3-horizontally)
(global-set-key "\C-x8" (lambda () (interactive) (split-window-n-horizontally 100 50)))
(global-set-key "\C-x9" 'setup-my-windows)
;(global-set-key [(ctrl meta ?_)] (lambda () (interactive) (safe-shrink-window 5)))
(global-set-key "\C-x_" 'my-display-at-bottom)

(defun my-swap-window-to-right (&optional below)
  "If swap buffer in this window with buffer on the right. If BELOW is set,
instead move current buffer to right and replace it with the next buffer from
the buffer stack in the current window."
  (interactive "P")
  (let ((cb (current-buffer))
        (cw (selected-window)))
    (if below
        (switch-to-buffer nil))
    (windmove-right)
    (if (not below)
        (set-window-buffer cw (current-buffer)))
    (switch-to-buffer cb)))

(defun my-swap-window-to-left (&optional below)
  (interactive "P")
  (let ((cb (current-buffer))
        (cw (selected-window)))
    (if below
        (switch-to-buffer nil))
    (windmove-left)
    (if (not below)
        (set-window-buffer cw (current-buffer)))
    (switch-to-buffer cb)))

(global-set-key "\C-x>" 'my-swap-window-to-right)
(global-set-key "\C-x<" 'my-swap-window-to-left)

(defun maximize-window-15 ()
  (interactive)
  (maximize-window (max 5 (/ (* (frame-height) 15) 100))))

(global-set-key [(ctrl meta ?+)]  'maximize-window-15)

(defun safe-max-window-horizontally ()
  (interactive)
  (let ((found nil)
        (width 0)
        (count 0)
        (current (selected-window)))
    (walk-windows (function (lambda (w)
                              (let ((e (window-edges w)))
                                (if (< (nth 1 e) window-min-height)
                                    (progn
                                      (setq width (+ width (window-width w))
                                            count (1+ count))
                                      (if (equal w current)
                                          (setq found t)))))))
                  'nomini)
    (if (not found)
        (error "Current window is not a top window"))
    (shrink-window-horizontally (- (- width (window-width) (* window-min-width (1- count)))))))

(defun safe-max-window ()
  (interactive)
  (maximize-window 5)
  (condition-case nil
      (safe-max-window-horizontally)
    (error nil)))

(global-set-key "\C-x=" 'safe-max-window)
(global-set-key "\C-x-" 'maximize-window-15)
