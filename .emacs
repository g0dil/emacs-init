(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/auto-install")

(require 'load-dir)
(load-dir "~/.emacs.d/setup")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-install-install-confirm nil)
 '(auto-install-replace-confirm nil)
 '(auto-install-save-confirm nil)
 '(auto-save-file-name-transforms (quote (("\\`.*\\([^/]*\\)\\'" "~/.emacs.d/autosave/\\1" t))))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(c-backslash-column 99)
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(comment-column 60)
 '(comment-fill-column 160)
 '(csv-align-padding 0)
 '(csv-comment-start-default "#")
 '(csv-field-quotes nil)
 '(csv-header-lines 0)
 '(csv-separators (quote ("|")))
 '(debug-on-error nil)
 '(default-input-method "latin-1-prefix")
 '(fill-column 98)
 '(flyspell-delay 10)
 '(global-auto-revert-mode nil)
 '(global-subword-mode t)
 '(global-visual-line-mode t)
 '(global-whitespace-mode nil)
 '(ido-mode (quote both) nil (ido))
 '(ispell-extra-args (quote ("--sug-mode=ultra")))
 '(ispell-program-name "c:/cygwin/bin/aspell.exe")
 '(ls-lisp-dirs-first t)
 '(nxml-where-global-mode t)
 '(nxml-where-header nil)
 '(pop-up-windows nil)
 '(save-place t nil (saveplace))
 '(server-done-hook (quote (delete-frame)))
 '(server-mode t)
 '(server-temp-file-regexp "^/tmp/Re\\|/draft\\|.*/itsalltext/.*$")
 '(server-window (quote switch-to-buffer-other-frame))
 '(show-paren-mode t)
 '(tab-width 4)
 '(tags-case-fold-search nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "outline" :family "Courier New"))))
 '(textile-acronym-face ((t (:foreground "medium blue"))))
 '(textile-blockquote-face ((t (:foreground "midnight blue"))))
 '(textile-code-face ((t (:foreground "firebrick4"))))
 '(textile-pre-face ((t (:foreground "dark green")))))
(put 'narrow-to-region 'disabled nil)
