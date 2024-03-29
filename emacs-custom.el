(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-install-install-confirm nil)
 '(auto-install-replace-confirm nil)
 '(auto-install-save-confirm nil)
 '(auto-save-file-name-transforms '(("\\`.*\\([^/]*\\)\\'" "~/.emacs.d/autosave/\\1" t)))
 '(auto-save-visited-mode nil)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function 'browse-url-chrome)
 '(browse-url-handlers '((".*" . browse-url-chrome)))
 '(c-backslash-column 99)
 '(c-basic-offset 4)
 '(c-offsets-alist '((arglist-intro . c-lineup-arglist-intro-after-paren)))
 '(column-number-mode t)
 '(comment-column 60)
 '(comment-fill-column 98)
 '(company-idle-delay 600)
 '(company-tooltip-idle-delay 10)
 '(compilation-ask-about-save nil)
 '(compilation-error-regexp-alist
   '(("^[       ]+\\([^:
]+\\):(?\\([0-9]+\\)" 1 2)
     ("^RUNTIME ERROR: +\\([^:
]+\\):(?\\([0-9]+\\)" 1 2)
     absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma msft edg-1 edg-2 epc ftnchek gradle-kotlin iar ibm irix java jikes-file maven jikes-line clang-include gcc-include ruby-Test::Unit gmake gnu lcc makepp mips-2 oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line
     ("^_\\{10,\\} \\([^ ]+\\.[^ ]+\\) _\\{10,\\}$" 1 nil)
     ("^\\([0-9]+\\): error: " nil 1)
     ("^STATIC ERROR: \\([^:]+\\):\\([^:]+\\):" 1 2)))
 '(compilation-scroll-output t)
 '(csv-align-padding 0)
 '(csv-comment-start-default "#")
 '(csv-field-quotes nil)
 '(csv-header-lines 0)
 '(csv-separators '("|"))
 '(dabbrev-abbrev-char-regexp "\\sw")
 '(debug-on-error nil)
 '(default-input-method "latin-1-prefix")
 '(develock-max-column-plist
   '(emacs-lisp-mode 99 lisp-interaction-mode w change-log-mode t texinfo-mode t c-mode 99 c++-mode 99 java-mode 99 jde-mode 99 html-mode 99 html-helper-mode 99 cperl-mode 99 perl-mode 99 mail-mode t message-mode t cmail-mail-mode t tcl-mode 99 ruby-mode 99))
 '(diff-mode-hook '((lambda nil (diff-auto-refine-mode 1))))
 '(diff-refine 'navigation)
 '(diff-switches "-u")
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(edit-server-done-hook nil)
 '(edit-server-new-frame nil)
 '(fill-column 98)
 '(flyspell-delay 30)
 '(flyspell-delayed-commands nil)
 '(frame-background-mode 'light)
 '(git-commit-fill-column 99)
 '(global-auto-revert-mode nil)
 '(global-highlight-changes-mode nil)
 '(global-subword-mode t)
 '(global-visual-line-mode nil)
 '(global-whitespace-mode t)
 '(gnus-autocheck-hook '(gnus-get-run-fetchmail))
 '(gnus-build-sparse-threads t)
 '(gnus-buttonized-mime-types '("multipart/signed"))
 '(gnus-fetch-old-headers nil)
 '(gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%O
")
 '(gnus-group-uncollapsed-levels 2)
 '(gnus-ignored-from-addresses
   "stefan@j32.de\\|stefan\\|stefan\\.bund@gmx\\.net\\|asbund@gmx\\.de")
 '(gnus-select-method '(nnml ""))
 '(gnus-summary-ignore-duplicates t)
 '(gnus-summary-line-format "%U%R%z %d %(%[%6L: %-32,32f%]%)%I %S
")
 '(gnus-treat-strip-leading-blank-lines t)
 '(gnus-treat-strip-trailing-blank-lines t)
 '(gnuserv-frame t)
 '(gnuserv-temp-file-regexp "^/tmp/.*\\|/draft\\|.*/.kde/share/apps/kfmexec/tmp/.*$")
 '(highlight-changes-face-list nil)
 '(highlight-changes-global-changes-existing-buffers t)
 '(hippie-expand-dabbrev-as-symbol t)
 '(hippie-expand-try-functions-list
   '(try-complete-file-name-partially try-complete-file-name try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-all-abbrevs try-expand-line try-expand-list))
 '(hs-isearch-open nil)
 '(ido-mode 'both nil (ido))
 '(indent-tabs-mode nil)
 '(ispell-dictionary "american")
 '(ispell-extra-args '("--sug-mode=ultra"))
 '(ispell-program-name "aspell")
 '(jsonnet-command "jsonnet")
 '(jsonnet-command-options '("--max-stack" "1000" "--ext-code" "PARAMETERS={}"))
 '(jsonnet-library-search-directories
   '("/home/stefan/group/Customers/PPI/src/financial-services/infrastructure/iac/lib/" "/home/stefan/group/Customers/PPI/src/financial-services/infrastructure/iac/lib/vendor"))
 '(line-move-visual nil)
 '(ls-lisp-dirs-first t)
 '(magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(magit-log-arguments '("-n256" "--graph" "--decorate"))
 '(magit-process-popup-time 1)
 '(magit-repository-directories '(("~/group/Customers/PPI/src" . 6)))
 '(magit-server-window-for-rebase 'pop-to-buffer)
 '(magit-status-buffer-switch-function 'switch-to-buffer)
 '(mail-alias-separator-string ", ")
 '(mail-complete-style 'angles)
 '(mail-dont-reply-to-names nil)
 '(mail-hist-keep-history t)
 '(mail-host-address "joshafat")
 '(mail-source-delete-incoming t)
 '(mail-source-directory "~/Gnus/")
 '(mail-source-movemail-program "/usr/bin/movemail")
 '(mail-source-primary-source nil)
 '(mail-sources
   '((file)
     (file :path "/net/maildirs/stefan/mail.spam")
     (file :path "/net/maildirs/stefan/mail.bulk")
     (file :path "/net/maildirs/stefan/mbox")
     (file :path "~/Gnus/in.rssfeed")
     (file :path "/net/maildirs/stefan/mail.probably-spam")))
 '(mail-specify-envelope-from t)
 '(makefile-backslash-column 96)
 '(markdown-command "markdown -f fencedcode")
 '(mc-pgp-always-sign t t)
 '(message-send-mail-function 'smtpmail-send-it)
 '(message-send-mail-partially-limit 10000000)
 '(mime-edit-split-message nil)
 '(mime-mailcap-file "~/.mailcap" t)
 '(mime-save-directory "~/tmp")
 '(mime-setup-default-signature-key "^C^S" t)
 '(multi-magit-repolist-columns
   '(("Name" 25 multi-magit-repolist-column-repo nil)
     ("Dirty" 5 multi-magit-repolist-column-status
      ((:right-align t)
       (:help-echo "N - untracked, U - unstaged, S - staged")))
     ("Branch" 25 magit-repolist-column-branch nil)
     ("B>U" 3 magit-repolist-column-unpushed-to-upstream
      ((:right-align t)
       (:help-echo "Local changes not in upstream")))
     ("U>B" 3 magit-repolist-column-unpulled-from-upstream
      ((:right-align t)
       (:help-echo "Remote changes not pulled")))
     ("Path" 99 magit-repolist-column-path nil)))
 '(multi-magit-status-sections-hook
   '(magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes multi-magit-insert-committed-changes))
 '(nnmail-active-file-coding-system 'raw-text)
 '(nnmail-list-identifiers '("\\[ecasound\\]" "\\[PyKDE\\]"))
 '(nnmail-pathname-coding-system nil)
 '(nnmail-split-methods 'nnmail-split-fancy)
 '(nxml-child-indent 2)
 '(nxml-where-global-mode nil)
 '(nxml-where-header nil)
 '(org-agenda-files '("~/Dropbox/Apps/Orgzly"))
 '(org-mode-hook
   '(#[nil "\305\306	>\203 \307
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
                                             `(face org-link mouse-face highlight keymap ,org-mouse-map)
                                             'prepend)
                                            t activate-bullets
                                            (("^[       ]*\\([-+*]\\|[0-9]+[.)]\\) +"
                                              (1
                                               `(face org-link keymap ,org-mouse-map mouse-face highlight)
                                               'prepend)))
                                            activate-checkboxes
                                            (("^[       ]*\\([-+*]\\|[0-9]+[.)]\\) +\\(\\[[ X]\\]\\)"
                                              (2
                                               `(face bold keymap ,org-mouse-map mouse-face highlight)
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
     org-babel-result-hide-spec org-babel-hide-all-hashes longlines-mode))
 '(org-startup-indented t)
 '(org-todo-keywords '((sequence "TODO" "NEXT" "DONE")))
 '(package-selected-packages
   '(fold-dwim fold-this dash rustic lsp-treemacs flycheck-rust cargo-transient cargo-mode cargo lsp-ui lsp-mode editorconfig-generate editorconfig rust-mode archive-phar adoc-mode ztree direnv terraform-mode go-mode python-docstring edit-indirect markdown-mode+ markdown-mode docker-compose-mode websocket typescript-mode kotlin-mode flycheck-kotlin))
 '(pop-up-windows nil)
 '(rmail-confirm-expunge 'y-or-n-p)
 '(rmail-decode-mime-charset t t)
 '(rmail-delete-after-output nil)
 '(rmail-displayed-headers nil)
 '(rmail-enable-mime nil)
 '(rmail-fields-not-to-output nil)
 '(rmail-highlighted-headers "^From:\\|^Subject:")
 '(rmail-mail-new-frame nil)
 '(rmail-retry-ignored-headers "^x-authentication-warning:")
 '(rmail-secondary-file-directory "~/")
 '(rmail-secondary-file-regexp "\\.xmail$")
 '(rmail-summary-line-count-flag t)
 '(rmail-summary-line-decoder 'identity)
 '(rmail-summary-scroll-between-messages t)
 '(rmail-user-mail-address-regexp nil)
 '(rng-validate-chunk-size 2000)
 '(rng-validate-delay 3)
 '(rng-validate-quick-delay 1.5)
 '(rtags-use-helm nil t)
 '(safe-local-variable-values
   '((jsonnet-library-search-directories "/home/stefan/group/Customers/PPI/src/financial-services/infrastructure/iac/google-cc/vendor" "/home/stefan/group/Customers/PPI/src/financial-services/infrastructure/iac/google-cc/lib")
     (jsonnet-library-search-directories "/home/stefan/group/Customers/PPI/src/financial-services/infrastructure/iac/google-cc/vendor")
     (eval ccide-find-build-script "tools/build-with-cmake.sh")
     (eval ccide-find-build-script "build-with-cmake.sh")
     (whitespace-style face trailing space-before-tab newline indentation empty space-after-tab lines-tail)
     (whitespace-style)
     (ispell-local-dictionary . de_DE-neu)
     (elisp-project-autoload-file-name . "sqi-autoload.el")
     (elisp-project-autoload-file-name . "cc-autoload.el")
     (ccide-auto-format-tag . "auto-uncrustify")
     (ccide-uncrustify-config . "~/src/search.uncrustify")
     (ccide-auto-format-tag "auto-uncrustify")
     (ccide-uncrustify-config "~/src/search.uncrustify")
     (whitespace-mode)))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(send-mail-function 'smtpmail-send-it)
 '(sendmail-coding-system nil t)
 '(server-done-hook nil)
 '(server-kill-new-buffers t)
 '(server-mode t)
 '(server-switch-hook nil)
 '(server-temp-file-regexp "^/tmp/Re\\|/draft\\|.*/itsalltext/.*$")
 '(server-window nil)
 '(show-paren-mode t)
 '(smerge-diff-switches '("-d" "-b" "-u"))
 '(smtpmail-auth-credentials '(("mail-out.j32.de" 250 "175087" "McYjtYU5")))
 '(smtpmail-debug-info t)
 '(smtpmail-default-smtp-server "mail-out.j32.de")
 '(smtpmail-local-domain "j32.de")
 '(smtpmail-sendto-domain "j32.de")
 '(smtpmail-smtp-server "mail-out.j32.de")
 '(smtpmail-starttls-credentials nil)
 '(speedbar-show-unknown-files nil)
 '(split-height-threshold 1000000)
 '(split-width-threshold 1000000)
 '(tab-width 8)
 '(tags-case-fold-search nil)
 '(text-scale-mode-step 1.09545)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(which-func-modes
   '(nxml-mode emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode python-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode diff-mode))
 '(which-function-mode nil)
 '(whitespace-action '(auto-cleanup warn-if-read-only))
 '(whitespace-line-column 100)
 '(whitespace-style
   '(face trailing space-before-tab newline indentation empty space-after-tab lines-tail)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(terraform--resource-name-face ((t (:foreground "orange red"))))
 '(textile-acronym-face ((t (:foreground "medium blue"))))
 '(textile-blockquote-face ((t (:foreground "midnight blue"))))
 '(textile-code-face ((t (:foreground "firebrick4"))))
 '(textile-pre-face ((t (:foreground "dark green")))))
(put 'narrow-to-region 'disabled nil)
