(require 'windmove)

(if window-system
    (progn
      (global-set-key (kbd "S-<left>") 'windmove-left)
      (global-set-key (kbd "S-<right>") 'windmove-right)
      (global-set-key (kbd "S-<up>") 'windmove-up)
      (global-set-key (kbd "S-<down>") 'windmove-down))
  (global-set-key (kbd "C-c <left>") 'windmove-left)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c <up>") 'windmove-up)
  (global-set-key (kbd "C-c <down>") 'windmove-down))
