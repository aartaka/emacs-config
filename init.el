;;=============================================================================
;; MELPA CONFIGURATIONS
;;=============================================================================

(require 'package)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;;===============================================================================
;; OTHER ESSENTIAL CONFIGURATIONS AND HELPERS
;;===============================================================================

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setf use-package-always-ensure t
        use-package-compute-statistics t))
(require 'diminish)
(require 'bind-key)

(use-package auto-package-update
  :config (auto-package-update-maybe)
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

(use-package autopair
  :config (autopair-global-mode t))

(use-package golden-ratio)
(use-package all-the-icons)
(use-package miniedit)

(use-package company :hook (after-init . global-company-mode)
  :config (add-to-list 'company-backends 'company-omnisharp))
(use-package company-quickhelp :hook (after-init . company-quickhelp-mode))
(use-package auto-complete)

(use-package keyfreq
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :hook ((after-init . keyfreq-mode)
         (after-init . keyfreq-autosave-mode)))

(use-package bbdb
  :config
  (progn
    (bbdb-initialize 'gnus 'message)
    (bbdb-mua-auto-update-init 'gnus 'message))
  :custom
  (bbdb-mua-auto-update-p t)
  (bbdb-mua-pop-up nil "I don't want BBDB to pop up anytime I read emails.")
  (bbdb-ignore-message-alist
   '(("From" . "donotreply")
     ("Mail-Followup-to" . "donotreply")
     ("Reply-to" . "donotreply")
     ("From" . "noreply")
     ("Mail-Followup-to" . "noreply")
     ("Reply-to" . "noreply")
     ("From" . "no-reply")
     ("Mail-Followup-to" . "no-reply")
     ("Reply-to" . "no-reply")
     ("From" . "no_reply")
     ("Mail-Followup-to" . "no_reply")
     ("Reply-to" . "no_reply")
     ("From" . "comments-noreply")
     ("Mail-Followup-to". "comments-noreply")
     ("Reply-to" . "comments-noreply")
     ("From" . "notification")
     ("Mail-Followup-to" . "notification")
     ("Reply-to" . "notification")
     ("From" . "notifications")
     ("Mail-Followup-to" . "notifications")
     ("Reply-to" . "notifications")
     ("From" . "info")
     ("Mail-Followup-to" . "info")
     ("Reply-to" . "info"))
   "Found somewhere on the Internet and altered to work with the services I use."))

(use-package erc
  :custom
  (erc-autojoin-channels-alist . ((list ".*\.freenode\.net" "lisp" "nyxt"))))
(use-package erc-colorize)
(use-package erc-image)

(use-package projectile
  :config (projectile-global-mode))

(use-package helm
  :init
  (progn
    (require 'helm-config)
    (if (version< "26.0.50" emacs-version)
        (eval-when-compile (require 'helm-lib)))

    (defun ar/helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))
    (defun ar/helm-eshell-enable-history ()
      (define-key eshell-mode-map (kbd "M-l")
        'helm-eshell-history))

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t)))
  :config
  (helm-mode)
  :custom
  (helm-lisp-fuzzy-completion t)
  (helm-scroll-amount 4)
  (helm-ff-search-library-in-sexp t)
  (helm-split-window-in-side-p t)
  (helm-echo-input-in-header-line t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-move-to-line-cycle-in-source t)
  (helm-buffer-skip-remote-checking t)
  (helm-mode-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-org-headings-fontify t)
  (helm-M-x-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  (helm-lisp-fuzzy-completion t)
  (helm-buffer-skip-remote-checking t)
  (helm-locate-fuzzy-match t)
  (helm-display-header-line nil)
  :hook ((helm-minibuffer-set-up . ar/helm-hide-minibuffer-maybe)
         (helm-goto-line-before . helm-save-current-pos-to-mark-ring)
         (eshell-mode . ar/helm-eshell-enable-history))
  :bind (("C-x c" . nil)
         ("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-d" . helm-browse-project)
         ("C-c r" . helm-recentf)
         :map helm-command-map
         ("C-h SPC" . helm-all-mark-rings)
         ("C-c h o" . helm-occur)
         ("C-c h w" . helm-wikipedia-suggest)
         ("C-c h g" . helm-google-suggest)
         ("C-c h x" . helm-register)
         ([remap list-buffers] . helm-buffers-list)
         :map minibuffer-local-map
         ("M-n" . helm-minibuffer-history)
         ("M-p" . helm-minibuffer-history)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to do persistent action
         ("C-i" . helm-execute-persistent-action)   ; make TAB works in terminal
         ("C-z" . helm-select-action)))             ; list actions using C-z

(use-package helm-projectile
  :requires (helm projectile)
  :config (helm-projectile-on)
  :custom
  (projectile-completion-system 'helm)
  (projectile-indexing-method 'alien))

(use-package helm-ag
  :custom
  (helm-ag-base-command "rg --no-heading")
  (helm-ag-fuzzy-match t)
  (helm-ag-success-exit-status '(0 2))
  :bind
  ("C-M-s" . helm-do-ag-project-root))

(use-package helm-swoop
  :requires helm
  :bind (("C-c h o" . helm-swoop)
         ("C-c h s" . helm-multi-swoop-all)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch) ; When doing isearch, hand the word over to helm-swoop
         :map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop)) ; From helm-swoop to helm-multi-swoop-all
  :custom
  (helm-multi-swoop-edit-save t) ; Save buffer when helm-multi-swoop-edit complete
  (helm-swoop-split-with-multiple-windows t) ; If this value is t, split window inside the current window
  (helm-swoop-split-direction 'split-window-vertically) ; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (helm-swoop-speed-or-color t)) ; If nil, you can slightly boost invoke speed in exchange for text color(use-package helm-swoop

(use-package yasnippet
  :config (yas-global-mode))
(use-package yasnippet-snippets :requires yasnippet)
(use-package yasnippet-classic-snippets :requires yasnippet)
(use-package helm-c-yasnippet
  :requires (yasnippet helm)
  :custom (helm-yas-space-match-any-greedy t)
  :bind ("C-c y" . helm-yas-complete))

(use-package emacs
  :init (progn
          (defun ar/show-trailing-whitespace ()
            (interactive)
            ;; Show unncessary whitespace that can mess up your diff
            (setf show-trailing-whitespace 1))
          (defun ar/set-frame-setting ()
            (interactive)
            (when (member "Hack" (font-family-list))
              (set-frame-font "Hack-17" t t))
            (tool-bar-mode -1)
            (menu-bar-mode -1)
            (scroll-bar-mode -1))
          (defun ar/browse-url-icecat (url &optional new-window)
            "See `browse-url-firefox' for reference"
            (interactive (browse-url-interactive-arg "URL: "))
            (setq url (browse-url-encode-url url))
            (let* ((process-environment (browse-url-process-environment)))
              (apply 'start-process
                     (concat "icecat " url) nil
                     browse-url-firefox-program
                     (append
                      browse-url-firefox-arguments
                      (if (browse-url-maybe-new-window new-window)
                          (if browse-url-firefox-new-window-is-tab
                              '("-new-tab")
                            '("-new-window")))
                      (list url)))))
          (require 'em-term)
          (require 'epa-file)
          (require 'dired))
  :config
  (progn
    ;; Configure contribs and non-MELPA packages path
    (add-to-list 'load-path "~/.emacs.d/lisp/")
    (let ((default-directory "~/.emacs.d/lisp/"))
      (normal-top-level-add-subdirs-to-load-path))
    ;; Misc customizationsn
    (fset 'yes-or-no-p 'y-or-n-p)        ;replace y-e-s by y
    (defconst query-replace-highlight t) ;highlight during query
    (defconst search-highlight t)        ;highlight incremental search

    (when window-system (ar/set-frame-setting))

    ;; Unicode support
    (set-language-environment "UTF-8")
    (set-default-coding-systems 'utf-8)

    ;; Use local Unicode data (possibly newer than upstream)
    (when (file-exists-p "~/.emacs.d/UnicodeData.txt")
      (setq describe-char-unicodedata-file "~/.emacs.d/UnicodeData.txt"))

    (set-fontset-font t '(#x1f300 . #x1fad0)
                      (when (member "Noto Emoji" (font-family-list))
                        "Noto Emoji"))
    (when (member "Hack" (font-family-list))
      (set-frame-font "Hack-17" t t)))
  :custom

  (backup-by-copying t)
  (backup-directory-alist
   '(("." . "~/.saves/")) "For backups to not clutter everything.")
  (delete-old-versions t "Delete old file backups silently.")
  (ls-lisp-dirs-first t "Display dirs first in dired.")
  (ecb-tip-of-the-day nil "Turn off ECB tips.")
  (split-height-threshold 0 "Split windows over horisontal line")
  (split-width-threshold nil "...and not opposite.")
  (inhibit-startup-screen t "No splash screen.")
  (initial-buffer-choice #'eshell "Startup on eshell.")
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (prefer-coding-system 'utf-8 "Prefer UTF-8.")
  (flyspell-issue-message-flag nil "Don't print per-word messages.")
  (normal-erase-is-backspace t "Fix weird backspace.")
  (browse-url-browser-function #'ar/browse-url-icecat)
  (indent-tabs-mode nil "Use space to indent by default.")
  (tab-width 4 "Set tab representation width to 4 spaces.")
  :bind (("C-x C-d" . dired)
         ("C-x C-b" . helm-buffers-list)
         ("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("<f12>" . eshell)
         ("C-z" . nil) ; Because suspend-emacs makes me mad in X
         ("C-c w" . whitespace-mode) ; Highligh whitespaces
         ("C-c ;" . comment-or-uncomment-region)) ; Set the commenting and uncommenting to the convenient keys
  :hook ((after-init . show-paren-mode)
         (after-init . auto-save-visited-mode)
         (after-init . global-font-lock-mode)
         (after-init . line-number-mode)
         (after-init . column-number-mode)
         (after-init . epa-file-enable)
         (prog-mode . ar/show-trailing-whitespace)
         (before-make-frame-hook . ar/set-frame-setting)))

(use-package column-enforce-mode
  :config (global-column-enforce-mode))

(use-package flyspell
  :hook ((markdown-mode . flyspell-mode)
         (text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode))
  :custom (ispell-list-command "--list" "EmacsWiki said it helps."))

;; Need to install pdf-tools for this to work
(pdf-loader-install)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(defvar org-directory "~/org")
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("M-q" . org-fill-paragraph))
  :custom
  (org-startup-with-inline-images t "Inline images in Org files!")
  (org-startup-with-latex-preview t "Inline LaTeX formulas!")
  (org-agenda-files `(,(concat org-directory "/schedule.org")
                      ,(concat org-directory "/tasks.org")
                      ,(concat org-directory "/notes.org")))
  (org-default-notes-file (concat org-directory "/notes.org") "I actually don't know why I need it.")
  (org-agenda-start-on-weekday nil "No thinking about the past.")
  (org-log-done 'note "Ask for closing note.")
  (org-log-redeadline 'note "Ask for rescheduling reason.")
  (org-hide-leading-stars t "I need only one star to know that it's heading.")
  (org-capture-templates
   `(("t" "Todo" entry
      (file+headline ,(concat org-directory "/tasks.org") "Todos")
      "** TODO %?\n  %i\n  %a")
     ("d" "Deadline" entry
      (file+headline ,(concat org-directory "/tasks.org") "Tasks")
      "** TODO %?\n   DEADLINE %^{Task deadline}T\n   %U")
     ("c" "Chaotic schedules" entry
      (file+headline ,(concat org-directory "/schedule.org") "Chaotic")
      "** %?\n %^{Date and Time}T\n")))
  (org-clock-persist 'history)
  :config (org-clock-persistence-insinuate))

(use-package ox-gfm
  :after org
  :config (add-to-list 'org-export-backends 'gfm))
(use-package ox-html5slide
  :after org
  :config (add-to-list 'org-export-backends 'html5slide))
(use-package ox-tiddly
  :after org
  :config (add-to-list 'org-export-backends 'tiddly))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config (setf vc-handled-backends nil))

(use-package clean-aindent-mode
  :bind ("RET" . newline-and-indent)
  :custom (clean-aindent-is-simple-indent t))

;;===============================================================================
;; LISP CUSTOMIZATIONS, WEEEEEEE!
;;===============================================================================

(require 'clhs)
(use-package sly
  :after clhs
  :config
  (progn
    (defun ar/hyperspec-lookup (symbol)
      (interactive (list (common-lisp-hyperspec-read-symbol-name)))
      (let ((browse-url-browser-function 'eww-browse-url))
        (hyperspec-lookup symbol)))

    (defun ar/hyperspec-lookup-reader-macro (macro)
      (interactive
       (list
        (let ((completion-ignore-case t))
          (completing-read "Look up reader-macro: "
                           common-lisp-hyperspnec--reader-macros nil t
                           (common-lisp-hyperspec-reader-macro-at-point)))))
      (let ((browse-url-browser-function 'eww-browse-url))
        (hyperspec-lookup-reader-macro macro)))

    (defun ar/hyperspec-lookup-format (character)
      (interactive (list (common-lisp-hyperspec--read-format-character)))
      (let ((browse-url-browser-function 'eww-browse-url))
        (hyperspec-lookup-format character)))

    (defun ar/set-lisp-indent ()
      (interactive)
      (set (make-local-variable lisp-indent-function)
           'common-lisp-indent-function))

    (defun ar/set-lisp-columns ()
      (interactive)
      (set-variable 'column-enforce-column 100 t)))
  :custom
  (inferior-lisp-program "sbcl")
  (sly-lisp-implementations
   '((sbcl ("sbcl") :coding-system utf-8-unix)
     (ccl "ccl")))
  :bind (("C-h -" . ar/hyperspec-lookup)
         ("C-h #" . ar/hyperspec-lookup-reader-macro)
         ("C-h ~" . ar/hyperspec-lookup-format))
  :hook ((lisp-mode . ar/set-lisp-columns)
         (lisp-mode . ar/set-lisp-indent)
         (sly-editing . company-mode)
         (sly-mode . golden-ratio-mode)))
(use-package sly-asdf)
(use-package sly-quicklisp)
(use-package helm-sly)

;; https://raw.githubusercontent.com/arclanguage/anarki/master/extras/inferior-arc.el
(require 'inferior-arc)
;; https://raw.githubusercontent.com/arclanguage/anarki/master/extras/arc.el
(require 'arc)
(add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))
(use-package racket-mode)
(use-package geiser
  :after sly
  :commands geiser-mode
  :hook ((scheme-mode . geiser-mode)
         (scheme-mode . ar/set-lisp-columns)
         (scheme-mode . golden-ration-mode)))

(use-package paredit
  :config
  (defun no-space-between-@-open-paren (endp delimiter)
    (not (and (eql ?\( delimiter)
              (eql ?\@ (char-before (point))))))
  :custom (paredit-space-for-delimiter-predicates
           '(no-space-between-@-open-paren))
  :commands paredit-mode
  :hook ((sly-mode . paredit-mode)
         (sly-editing . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (geiser-mode . paredit-mode)
         (arc-mode . paredit-mode)))

(use-package paredit-everywhere
  :hook (prog-mode . paredit-everywhere-mode))

;;==============================================================================
;; C/C++ CUSTOMISATIONS
;;==============================================================================

(use-package cc-mode
  :config (setf (cdr (assoc 'other c-default-style)) "linux")
  :custom
  (c-basic-offset 8)
  ;; use gdb-many-windows by default
  (gdb-many-windows t)
  ;; Non-nil means display source file containing the main routine at startup
  (gdb-show-main t)
  :bind (("<f5>" . ar/compile)
         :map c-mode-map
         ("<tab>" . company-indent-or-complete-common)
         :map c++-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :hook ((c-mode-common . hs-minor-mode)))

;;==============================================================================
;; C# CUSTOMISATIONS
;;==============================================================================

(use-package omnisharp
  :config (omnisharp-install-server nil)
  :hook ((csharp-mode . omnisharp-mode)
         (csharp-mode . company-mode)
         (csharp-mode . flycheck-mode))
  :custom (omnisharp-debug t)
  :bind (:map omnisharp-mode-map
         ("C-c r r" . omnisharp-run-code-action-refactoring)
         ("C-c C-c" . recompile)))

;;==============================================================================
;; PYTHON CUSTOMIZATIONS
;;==============================================================================

(use-package elpy
  :init (defun ar/switch-company-to-ac ()
          (interactive)
          ;; Elpy/EIN is not in good rels with Company somewhy
          (company-mode -1)
          (auto-complete-mode +1))
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-prompt-detect-failure-warning nil)
  :config
  (progn (elpy-enable)
         (add-to-list 'python-shell-completion-native-disabled-interpreters
                      "jupyter"))
  :hook (elpy-mode . ar/switch-company-to-ac))

(use-package ein
  :config (progn
            (require 'ein-notebook)
            (require 'ein-subpackages))
  :hook (ein:notebook-python-mode . ar/switch-company-to-ac))

(use-package flycheck
  :requires (elpy ein)
  :custom (elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :hook ((ein:notebook-python-mode . flycheck-mode)
         (elpy-mode . flycheck-mode)))

(use-package py-autopep8
  :requires (elpy ein)
  :hook ((elpy-mode . py-autopep8-enable-on-save)
         (ein:notebook-python-mode . py-autopep8-enable-on-save)))

(use-package blacken
  :requires (elpy ein)
  :hook ((elpy-mode . blacken-mode)
         (ein:notebook-python-mode . blacken-mode)))

;;=============================================================================
;; WEB-DEVELOPMENT CUSTOMIZATIONS
;;=============================================================================

(use-package web-mode)
(use-package js2-mode)
(use-package skewer-less
  :init (defun ar/browse-this-file (file)
          (interactive (list (buffer-file-name)))
          (unless (and file (file-exists-p file))
            (error "File does not exist: ‘%s’" file))
          (unless (process-status "httpd")
            (httpd-start))
          (setf httpd-root (file-name-directory file))
          (browse-url (format "http://127.0.0.1:%s/%s" httpd-port
                              (file-name-nondirectory file))))
  :requires js2-mode
  :hook ((html-mode . skewer-html-mode)
         (js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (html-mode . httpd-start)))

(use-package rainbow-mode
  :hook (html-mode . rainbow-mode))

;;==============================================================================
;; LOOK CUSTOMIZATIONS
;;==============================================================================

;; Just in case I'd switch back to Tomorrow Night
(use-package base16-theme)

(use-package monochrome-theme
  :config
  (load-theme 'monochrome t)
  (load-theme 'aartaka-monochrome t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "ef403aa0588ca64e05269a7a5df03a5259a00303ef6dfbd2519a9b81e4bce95c" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "a6764d1da0588bf27632f3db4078f9174f2cf1fc1d9cab97eec27b0a712d7518" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" default))
 '(eshell-visual-commands
   '("vi" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm" "protonvpn"))
 '(org-export-with-toc nil)
 '(package-selected-packages
   '(omnisharp ox-gfm bbdb pretty-sha-path miniedit web-mode yasnippet-classic-snippets yasnippet-snippets yasnippet-lean skewer-less mmm-mode skewer rainbow-mode keyfreq all-the-icons nov pdf-tools pdfgrep esup elisp--witness--lisp flymake-racket racket-mode ggtags helm-gtags use-package w3 base16-theme autopair))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
