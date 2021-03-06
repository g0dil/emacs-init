(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-install-install-confirm nil)
 '(auto-install-replace-confirm nil)
 '(auto-install-save-confirm nil)
 '(auto-save-file-name-transforms
   (quote
    (("\\`.*\\([^/]*\\)\\'" "~/.emacs.d/autosave/\\1" t))))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(c-backslash-column 99)
 '(c-basic-offset 4)
 '(c-offsets-alist (quote ((arglist-intro . +))))
 '(column-number-mode t)
 '(comment-column 60)
 '(comment-fill-column 98)
 '(company-idle-delay 600)
 '(company-tooltip-idle-delay 10)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(csv-align-padding 0)
 '(csv-comment-start-default "#")
 '(csv-field-quotes nil)
 '(csv-header-lines 0)
 '(csv-separators (quote ("|")))
 '(dabbrev-abbrev-char-regexp "\\sw")
 '(debug-on-error nil)
 '(default-input-method "latin-1-prefix")
 '(develock-max-column-plist
   (quote
    (emacs-lisp-mode 99 lisp-interaction-mode w change-log-mode t texinfo-mode t c-mode 99 c++-mode 99 java-mode 99 jde-mode 99 html-mode 99 html-helper-mode 99 cperl-mode 99 perl-mode 99 mail-mode t message-mode t cmail-mail-mode t tcl-mode 99 ruby-mode 99)))
 '(diff-mode-hook (quote ((lambda nil (diff-auto-refine-mode 1)))))
 '(diff-switches "-u")
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(edit-server-done-hook nil)
 '(edit-server-new-frame nil)
 '(fill-column 98)
 '(flyspell-delay 30)
 '(flyspell-delayed-commands nil)
 '(frame-background-mode (quote light))
 '(git-commit-fill-column 99)
 '(global-auto-revert-mode nil)
 '(global-highlight-changes-mode nil)
 '(global-subword-mode t)
 '(global-visual-line-mode nil)
 '(global-whitespace-mode t)
 '(highlight-changes-face-list nil)
 '(highlight-changes-global-changes-existing-buffers t)
 '(hippie-expand-dabbrev-as-symbol nil)
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-all-abbrevs try-expand-line try-expand-list)))
 '(hs-isearch-open nil)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(ispell-dictionary "american")
 '(ispell-extra-args (quote ("--sug-mode=ultra")))
 '(ispell-program-name "aspell")
 '(line-move-visual nil)
 '(ls-lisp-dirs-first t)
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1))
 '(magit-log-arguments (quote ("-n256" "--graph" "--decorate")))
 '(magit-process-popup-time 1)
 '(magit-server-window-for-rebase (quote pop-to-buffer))
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(nxml-child-indent 2)
 '(nxml-where-global-mode nil)
 '(nxml-where-header nil)
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-mode-hook
   (quote
    (#[nil "\305\306	>\203 \307
\310\311#\210\307\312\313#\210\307\314\315#\210\306	>\203, \307
\316\317#\210\307
\320\321#\210\322	>\203> \307\323\324#\210\307\325\324#\210\326	>\203P \307
\327\317#\210\307
\330\321#\210\331	>\203_ \332\311\333BC\334#\210\335	>\203k \332\311\336\334#\210\337	>\203w \332\311\340\334#\210\341\342\343\344\311$\210\345\342\311\"\210\342\207"
           [org-mouse-context-menu-function org-mouse-features org-mouse-map org-mode-map org-outline-regexp org-mouse-context-menu context-menu org-defkey
                                            [mouse-3]
                                            nil
                                            [mouse-3]
                                            org-mouse-show-context-menu
                                            [down-mouse-1]
                                            org-mouse-down-mouse
                                            [C-drag-mouse-1]
                                            org-mouse-move-tree
                                            [C-down-mouse-1]
                                            org-mouse-move-tree-start yank-link
                                            [S-mouse-2]
                                            org-mouse-yank-link
                                            [drag-mouse-3]
                                            move-tree
                                            [drag-mouse-3]
                                            [down-mouse-3]
                                            activate-stars font-lock-add-keywords
                                            (0
                                             (\`
                                              (face org-link mouse-face highlight keymap
                                                    (\, org-mouse-map)))
                                             (quote prepend))
                                            t activate-bullets
                                            (("^[       ]*\\([-+*]\\|[0-9]+[.)]\\) +"
                                              (1
                                               (\`
                                                (face org-link keymap
                                                      (\, org-mouse-map)
                                                      mouse-face highlight))
                                               (quote prepend))))
                                            activate-checkboxes
                                            (("^[       ]*\\([-+*]\\|[0-9]+[.)]\\) +\\(\\[[ X]\\]\\)"
                                              (2
                                               (\`
                                                (face bold keymap
                                                      (\, org-mouse-map)
                                                      mouse-face highlight))
                                               t)))
                                            ad-add-advice org-open-at-point
                                            (org-mouse-open-at-point nil t
                                                                     (advice lambda nil
                                                                             (let
                                                                                 ((context
                                                                                   (org-context)))
                                                                               (cond
                                                                                ((assq :headline-stars context)
                                                                                 (org-cycle))
                                                                                ((assq :checkbox context)
                                                                                 (org-toggle-checkbox))
                                                                                ((assq :item-bullet context)
                                                                                 (let
                                                                                     ((org-cycle-include-plain-lists t))
                                                                                   (org-cycle)))
                                                                                ((org-footnote-at-reference-p)
                                                                                 nil)
                                                                                (t ad-do-it)))))
                                            around ad-activate]
           5]
     my-setup-org
     #[nil "\300\301\302\303\304$\207"
           [org-add-hook change-major-mode-hook org-show-block-all append local]
           5]
     #[nil "\300\301\302\303\304$\207"
           [org-add-hook change-major-mode-hook org-babel-show-result-all append local]
           5]
     org-babel-result-hide-spec org-babel-hide-all-hashes longlines-mode)))
 '(org-startup-indented t)
 '(pop-up-windows nil)
 '(rng-validate-chunk-size 2000)
 '(rng-validate-delay 3)
 '(rng-validate-quick-delay 1.5)
 '(rtags-use-helm nil t)
 '(safe-local-variable-values
   (quote
    ((ispell-local-dictionary . de_DE-neu)
     (elisp-project-autoload-file-name . "sqi-autoload.el")
     (elisp-project-autoload-file-name . "cc-autoload.el")
     (ccide-auto-format-tag . "auto-uncrustify")
     (ccide-uncrustify-config . "~/src/search.uncrustify")
     (ccide-auto-format-tag "auto-uncrustify")
     (ccide-uncrustify-config "~/src/search.uncrustify")
     (whitespace-mode))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(server-done-hook nil)
 '(server-kill-new-buffers t)
 '(server-mode t)
 '(server-switch-hook nil)
 '(server-temp-file-regexp "^/tmp/Re\\|/draft\\|.*/itsalltext/.*$")
 '(server-window nil)
 '(show-paren-mode t)
 '(smerge-diff-switches (quote ("-d" "-b" "-u")))
 '(split-height-threshold 1000000)
 '(split-width-threshold 1000000)
 '(tab-width 8)
 '(tags-case-fold-search nil)
 '(text-scale-mode-step 1.09545)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(which-func-modes
   (quote
    (nxml-mode emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode python-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode diff-mode)))
 '(which-function-mode nil)
 '(whitespace-action (quote (auto-cleanup warn-if-read-only)))
 '(whitespace-line-column 100)
 '(whitespace-style
   (quote
    (face trailing space-before-tab newline indentation empty space-after-tab lines-tail)))
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(textile-acronym-face ((t (:foreground "medium blue"))))
 '(textile-blockquote-face ((t (:foreground "midnight blue"))))
 '(textile-code-face ((t (:foreground "firebrick4"))))
 '(textile-pre-face ((t (:foreground "dark green")))))
(put 'narrow-to-region 'disabled nil)
