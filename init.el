(require 'package)

(add-to-list 'load-path "~/.config/emacs/lisp/")
(add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp/")

(add-to-list 'load-path "~/.config/emacs/stimmung-themes/")
(require 'stimmung-themes)
(setf stimmung-themes-dark-highlight-color "#330101")
(load-file "~/.config/emacs/stimmung-themes/stimmung-themes-dark-theme.el")
(load-theme 'stimmung-themes-dark t)

(add-to-list 'load-path "~/git/nyxt/build-scripts/")
(require 'nyxt-guix)
(add-to-list 'load-path "~/git/moirai.el/")
(let ((default-directory "~/.config/emacs/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Emacspeak setup. Commented out because I don't use it often.
(setq espeak-default-speech-rate 230)
;; (load-file "~/.guix-extra-profiles/emacs-profile/emacs-profile/share/emacs/site-lisp/emacspeak/lisp/emacspeak-setup.el")

(global-unset-key [\C-e \C-b])

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

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

;;==============================================================================
;; OTHER ESSENTIAL CONFIGURATIONS AND HELPERS
;;==============================================================================

(use-package golden-ratio
  :diminish golden-ratio-mode)
(use-package all-the-icons)
(use-package miniedit)

(use-package helm-company)
(use-package company
  :diminish company-mode
  :requires (helm-company)
  :hook (after-init . global-company-mode)
  :custom (company-idle-delay 0.01)
  :bind (:map company-mode-map
         ("C-:" . 'helm-company)
         :map company-active-map
         ("C-:" . 'helm-company)))
(use-package company-quickhelp
  :hook (after-init . company-quickhelp-mode))
(use-package auto-complete)

(use-package wordnut
  :bind ("C-c d" . wordnut-search))
(use-package synosaurus
  :bind ("C-c t" . synosaurus-lookup))

(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode))
(use-package yasnippet-snippets)
(use-package yasnippet-classic-snippets)
(use-package helm-c-yasnippet
  :custom (helm-yas-space-match-any-greedy t)
  :bind ("C-c y" . helm-yas-complete))

(eval-when-compile
  (pinentry-start))

(use-package emacs
  :init (progn
          (defun ar/show-trailing-whitespace ()
            (interactive)
            ;; Show unncessary whitespace that can mess up your diff
            (setf show-trailing-whitespace 1))
          (defun ar/set-frame-setting ()
            (interactive)
            (tool-bar-mode -1)
            (menu-bar-mode -1)
            (scroll-bar-mode -1))
          (defun ar/browse-url-nyxt (url &optional new-window)
            "See `browse-url-firefox' for reference"
            (interactive (browse-url-interactive-arg "URL: "))
            (setq url (browse-url-encode-url url))
            (let* ((process-environment (browse-url-process-environment)))
              (apply 'start-process
                     "Nyxt" nil "nyxt" (list url))))
          (require 'em-term))
  :config
  (progn
    ;; Misc customizationsn
    (fset 'yes-or-no-p 'y-or-n-p)        ;replace y-e-s by y
    (defconst query-replace-highlight t) ;highlight during query
    (defconst search-highlight t)        ;highlight incremental search

    (when window-system (ar/set-frame-setting))

    (if (member "IBM Plex Mono" (font-family-list))
        (set-frame-font "IBM Plex Mono-17" t t)
      (set-frame-font "17" t t)))
  :custom
  (save-interprogram-paste-before-kill t "Integrate system clipboard into Emacs.")
  (visible-bell t "This beeping sound annoys me so much...")
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
  (browse-url-browser-function #'ar/browse-url-nyxt)
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
         (after-init . global-auto-revert-mode)
         (prog-mode . ar/show-trailing-whitespace)
         (before-make-frame-hook . ar/set-frame-setting)))

(use-package column-enforce-mode
  :diminish column-enforce-mode
  :config (global-column-enforce-mode))

(use-package eldoc
  :diminish eldoc-mode)

(use-package flyspell
  :diminish flyspell-mode
  :hook ((markdown-mode . flyspell-mode)
         (text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode))
  :custom (ispell-list-command "--list" "EmacsWiki said it helps."))

;; Need to install pdf-tools for this to work
(pdf-loader-install)

(use-package emms
  :config
  (emms-all)
  (emms-default-players))

;;; Not exactly related to pdf-tools, but let it be there
(defun unpdf ()
  "Run pdftotext on the entire buffer."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (buffer (get-buffer-create
                  (concat "*unpdf:" (file-name-nondirectory file-name) "*"))))
    (with-current-buffer buffer
      (shell-command
       (format "pdftotext \"%s\" -"
               file-name)
       (current-buffer)
       t)
      (switch-to-buffer buffer))))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package transient)

(use-package magit
  :after transient
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config (setf vc-handled-backends nil))
(use-package clean-aindent-mode
  :bind ("RET" . newline-and-indent)
  :custom (clean-aindent-is-simple-indent t))

(use-package keepass-mode)

;;==============================================================================
;; SOCIAL
;;==============================================================================

(require 'mu4e)
(load "~/.config/emacs/lisp/mu4e-config.el")
(require 'mu4e-config)

(defun ar/mu4e-set-account ()
  "Set the account for composing a message.
This function is taken from:
http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
          (completing-read (format "Compose with account: (%s) "
                                   (mapconcat #'(lambda (var) (car var))
                                              ar/mu4e-account-alist "/"))
                           (mapcar #'(lambda (var) (car var)) ar/mu4e-account-alist)
                           nil t nil nil (caar ar/mu4e-account-alist)))
         (account-vars (cdr (assoc account ar/mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'ar/mu4e-set-account)

(use-package bbdb
  :config
  (progn
    (bbdb-initialize 'mu4e 'message)
    (bbdb-mua-auto-update-init 'mu4e 'message))
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
     ("Reply-to" . "info")
     ("From" . "Info")
     ("Mail-Followup-to" . "Info")
     ("Reply-to" . "Info")
     ("From" . "drive-shares-noreply")
     ("Mail-Followup-to" . "drive-shares-noreply")
     ("Reply-to" . "drive-shares-noreply")
     ("From" . "notifications")
     ("Mail-Followup-to" . "notifications")
     ("Reply-to" . "notifications"))
   "Found somewhere on the Internet and altered to work with the services I use."))

(use-package erc
  :custom
  (erc-autojoin-channels-alist . ((list ".*\.libera.chat" "lisp" "nyxt"))))
(use-package erc-colorize)
(use-package erc-image)

(use-package telega)

;;==============================================================================
;; HELM AND FRIENDS
;;==============================================================================

(use-package helm
  :diminish helm-mode
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

(use-package projectile
  :diminish projectile-mode
  :config (projectile-global-mode))

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
  (helm-ag-insert-at-point 'symbol)
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

;;==============================================================================
;; ORG
;;==============================================================================

(defvar org-directory "~/org")
(use-package org
  :init
  (defun insert-em-dash ()
    (interactive)
    (insert-char #x2014))
  (defun insert-right-angle-quote ()
    (interactive)
    (insert-char #xBB))
  (defun insert-left-angle-quote ()
    (interactive)
    (insert-char #xAB))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-'" . insert-left-angle-quote)
         ("C-M-'" . insert-right-angle-quote)
         ("C-M--" . insert-em-dash)
         :map org-mode-map
         ("C-'" . insert-left-angle-quote)
         ("M-q" . org-fill-paragraph))
  :custom
  (org-startup-with-inline-images t "Inline images in Org files!")
  (org-startup-with-latex-preview t "Inline LaTeX formulas!")
  (org-hide-emphasis-markers t "Hide the markup to have make it prettier.")
  (org-hidden-keywords '(author date email title) "Don't show the obvious keywords.")
  (org-agenda-files `(,(concat org-directory "/schedule.org")
                      ,(concat org-directory "/tasks.org")
                      ,(concat org-directory "/notes.org")))
  (org-default-notes-file (concat org-directory "/notes.org") "I actually don't know why I need it.")
  (org-agenda-start-on-weekday nil "No thinking about the past.")
  (org-agenda-span 20 "How far do I look in the future?")
  (org-log-done 'note "Ask for closing note.")
  (org-log-redeadline 'note "Ask for rescheduling reason.")
  (org-hide-leading-stars t "I need only one star to know that it's heading.")
  (org-capture-templates
   `(("d" "Deadline" entry
      (file+headline ,(concat org-directory "/tasks.org") "Todos")
      "** TODO %?\n   DEADLINE %^{Task deadline}T\n   %U")
     ("c" "Chaotic schedules" entry
      (file+headline ,(concat org-directory "/schedule.org") "Chaotic")
      "** %? %^{Date and Time}T\n")))
  (org-clock-persist 'history)
  (org-latex-default-packages-alist
   '(("T2A,OT1"   "fontenc"   t)
     ("utf8" "inputenc"  t)
     ("russian" "babel" t)
     (""     "graphicx"  t)
     (""     "grffile"   t)
     (""     "longtable" nil)
     (""     "wrapfig"   nil)
     (""     "rotating"  nil)
     ("normalem" "ulem"  t)
     (""     "amsmath"   t)
     (""     "textcomp"  t)
     (""     "amssymb"   t)
     (""     "capt-of"   nil)
     (""     "titletoc"  nil)
     (""     "hyperref"  nil)
     (""     "cleveref"  nil)))
  (org-latex-packages-alist
   `(,@org-latex-packages-alist
     ("" "tabu" t)))
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  (org-latex-pdf-process '("xelatex -etex -interaction=nonstopmode -output-directory=%o %f"
                           "bibtex %b"
                           ;; "biber --output-directory %o $(basename %f .tex)"
                           "xelatex -etex -interaction=nonstopmode -output-directory=%o %f"
                           "xelatex -etex -interaction=nonstopmode -output-directory=%o %f")
                         "I need to cite things via BibTeX")
  (org-latex-with-hyperref t)
  (org-latex-prefer-user-labels t)
  (org-ref-show-broken-links t "To troubleshoot the broken links.")
  :config
  (org-clock-persistence-insinuate)
  (require 'ox-beamer)
  (unless (assoc "extarticle" org-latex-classes)
    (add-to-list 'org-latex-classes
                 '("extarticle"
                   "\\documentclass[14pt]{extarticle}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (setf (cadr (assoc* "beamer" org-latex-classes :test 'equalp))
             "\\documentclass[presentation,xcolor=x11names]{beamer}")
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)))

(use-package babel
  :config
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (css . t)
       (ditaa . t)
       (calc . t)
       (emacs-lisp . t)
       (dot . t)
       (js . t)
       (latex . t)
       (lisp . t)
       (org . t)
       (python . t)
       (scheme . t)
       (sql . t)
       (sqlite . t)))))

(use-package ox-gfm
  :after org
  :config (add-to-list 'org-export-backends 'gfm))
(use-package ox-twbs
  :after org
  :config (add-to-list 'org-export-backends 'twbs))
(use-package ox-tiddly
  :after org
  :config (add-to-list 'org-export-backends 'tiddly))

(use-package org-ref
  :init
  (progn
   (setq reftex-default-bibliography '("~/Documents/bibtex/bibliography.bib")
         org-ref-bibliography-notes "~/Documents/bibtex/notes.org"
         org-ref-default-bibliography reftex-default-bibliography
         org-ref-pdf-directory "~/Documents/bibtex/bibtex-pdfs/"
         bibtex-completion-bibliography '("~/Documents/bibtex/bibliography.bib")
         bibtex-completion-library-path "~/Documents/bibtex/bibtex-pdfs"
         bibtex-completion-notes-path "~/Documents/bibtex/helm-bibtex-notes"))
  :config
  (progn
    (require 'org-ref-pdf)
    (require 'org-ref-url-utils))
  :bind ("C-c j" . org-ref-cite-insert-helm)
  :custom
  (tex-bibtex-command "biber")
  (tex-run-command "pdftex")
  (latex-run-command "pdflatex"))

(use-package markdown-mode)
(use-package edit-indirect)

;;===============================================================================
;; LISP CUSTOMIZATIONS, WEEEEEEE!
;;===============================================================================

(use-package sly
  :init
  (progn
    (defun ar/set-lisp-indent ()
      (interactive)
      (set (make-local-variable lisp-indent-function)
           'common-lisp-indent-function))

    (defun ar/set-lisp-columns ()
      (interactive)
      (set-variable 'column-enforce-column 100 t)))
  :config
  (progn
    (require 'clhs)
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
    (setq sly-lisp-implementations
          `((sbcl ("sbcl" "--dynamic-space-size" "3000") :coding-system utf-8-unix)
            (sbcl-plain ("sbcl") :coding-system utf-8-unix)
            (sbcl-nyxt
             (lambda ()
               (nyxt-make-guix-sbcl-for-nyxt
                "~/git/nyxt"
                :ad-hoc '("guix" "gnupg" "git" "xdg-utils" "pkg-config"
                          "keepassxc" "gedit" "xclip" "gcc-toolchain")
                :no-grafts t)))
            (sbcl-nyxt-force
             (lambda ()
               (nyxt-make-guix-sbcl-for-nyxt
                "~/git/nyxt"
                :ad-hoc '("guix" "gnupg" "git" "xdg-utils" "pkg-config"
                          "keepassxc" "gedit" "xclip" "gcc-toolchain")
                :no-grafts t
                :force t)))
            (ecl  ("ecl"))
            (ccl  ("ccl")))))
  :custom
  (inferior-lisp-program "sbcl")
  (sly-connection-update-interval 0.1)
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

(use-package racket-mode)
(use-package geiser
  :after sly
  :commands geiser-mode
  :hook ((scheme-mode . geiser-mode)
         (scheme-mode . golden-ratio-mode)))

(use-package paredit
  :diminish paredit-mode
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
  :diminish paredit-everywhere-mode
  :hook (prog-mode . paredit-everywhere-mode))

(use-package clojure-mode
  :interpreter
  ("\\.edn$" . clojure-mode)
  ("\\.boot$" . clojure-mode)
  ("\\.cljs.*$" . clojure-mode)
  :hook
  (clojure-mode . paredit-mode)
  (clojure-mode . subword-mode))
(use-package clojure-mode-extra-font-locking)

(use-package parseclj)

(use-package cider
  :config
  (defun cider-refresh ()
    (interactive)
    (cider-interactive-eval (format "(user/reset)")))
  :custom
  (cider-repl-pop-to-buffer-on-connect t)
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)
  (cider-repl-history-file "~/.config/emacs/cider-history")
  (cider-repl-wrap-history t)
  :hook
  (cider-mode . eldoc-mode)
  (cider-repl-mode . paredit-mode)
  :bind
  (:map clojure-mode-map
   ("C-M-r" . cider-refresh)))

;;==============================================================================
;; C/C++ CUSTOMISATIONS
;;==============================================================================

(use-package hideshow
  :diminish hs-minor-mode
  :custom
  (hs-special-modes-alist
           (append '((csharp-mode "{" "}" "/[*/]" nil nil))
                   hs-special-modes-alist)
           "Hide C# blocks.")
  (hs-isearch-open t "Set whether isearch opens folded blocks.")
  (hs-hide-comments t "Hide the comments too when you do hs-hide-all.")
  :bind ("C--" . hs-toggle-hiding)
  :hook (hs-minor-mode . hs-hide-all))

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
  :hook (c-mode-common . hs-minor-mode))

(use-package nhexl-mode
  :custom
  (nhexl-display-unprintables t))

(use-package go-mode)
(use-package flymake-go)

(use-package brainfuck-mode
  :after hideshow
  :config
  (defun bf-setup ()
    (setf comment-start ";"
          comment-end "")
    (setf (buffer-local-value 'indent-line-function (current-buffer))
          (lambda ()
            (unless (= (point) (point-min))
              (let* ((text-before (subseq (buffer-string) (point-min) (1- (point-at-bol))))
                     (opening (count ?\[ text-before))
                     (old-point (point))
                     (old-indent
                      (progn (beginning-of-line)
                             (skip-chars-forward " \t")
                             (point)))
                     (closing (+ (count ?\] text-before)
                                 (if (= (char-after) ?\])
                                     1 0)))
                     (depth
                      (1+ (- opening closing)))
                     (new-indent (progn
                                   (indent-line-to depth)
                                   (point))))
                (setf (point) (+ old-point (- new-indent old-indent))))))))
  (add-to-list 'hs-special-modes-alist
               '(brainfuck-mode "\\[" "\\]" ";" nil))
  :hook
  (brainfuck-mode . hs-minor-mode)
  (brainfuck-mode . bf-setup))

;;==============================================================================
;; C# CUSTOMISATIONS
;;==============================================================================

(use-package csharp-mode
  :init (defun ar/set-csharp-style ()
          (interactive)
          (c-set-style "csharp"))
  :hook ((csharp-mode . ar/set-csharp-style)
         (csharp-mode . hs-minor-mode)))

;; (use-package omnisharp
;;   :config (omnisharp-install-server nil)
;;   :hook ((csharp-mode . omnisharp-mode)
;;          (csharp-mode . company-mode)
;;          (csharp-mode . flycheck-mode))
;;   :custom (omnisharp-debug t)
;;   :bind (:map omnisharp-mode-map
;;          ("C-c r r" . omnisharp-run-code-action-refactoring)
;;          ("C-c C-c" . recompile)))

;;==============================================================================
;; PYTHON CUSTOMIZATIONS
;;==============================================================================

(use-package elpy
  :custom
  (elpy-get-info-from-shell t)
  (elpy-rpc-python-command "python3")
  (python-shell-interpreter "python3")
  (python-shell-prompt-detect-failure-warning nil)
  :config
  (progn (elpy-enable)
         (add-to-list 'python-shell-completion-native-disabled-interpreters
                      "jupyter"))
  :hook (python-mode . elpy-mode))

(advice-add
 'elpy-doc :around
 (lambda (orig &rest args)
   (condition-case nil
       (apply orig args)
     (error
      (elpy-doc--show
       (python-shell-send-string-no-output
        (concat "help(" (thing-at-point 'filename) ")")))))))

(defun run-django-python ()
  "Run a python shell tailor-made for Django development."
  (interactive)
  (run-python
   (concat
    "guix shell -m " (concat "\"" (projectile-project-root) "manifest.scm\"")
    " -- python3 "
    (concat "\"" (projectile-project-root) "manage.py\"")
    " shell -i python")))

(use-package ein
  :config (require 'ein-notebook)
  :hook (ein:notebook-python-mode . ar/switch-company-to-ac))

(use-package flycheck
  :diminish flycheck-mode
  :requires (elpy ein)
  :init (require 's)
  :config
  (setq flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :custom
  (elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :hook ((after-init . global-flycheck-mode)
         (ein:notebook-python-mode . flycheck-mode)
         (elpy-mode . flycheck-mode)))

(use-package py-autopep8
  :requires (elpy ein)
  :hook ((elpy-mode . py-autopep8-enable-on-save)
         (ein:notebook-python-mode . py-autopep8-enable-on-save)))

;;=============================================================================
;; WEB-DEVELOPMENT CUSTOMIZATIONS
;;=============================================================================

(use-package web-mode
  :mode ("\\.jsx?$" . web-mode)
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-markup-indent-offset 2))
(use-package js2-mode
  :hook (web-mode . js2-minor-mode))
(use-package rjsx-mode)
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

(use-package add-node-modules-path
  :hook (flycheck-mode . add-node-modules-path))

(use-package rainbow-mode
  :hook
  (html-mode . rainbow-mode)
  (web-mode . rainbow-mode))

(use-package sqlup-mode
  :hook (sql-interactive-mode . sqlup-mode))

;;==============================================================================
;; LOOK CUSTOMIZATIONS
;;==============================================================================

(use-package smart-mode-line
  :config (sml/setup)
  :custom
  (sml/theme 'respectful)
  (sml/no-confirm-load-theme t))

;; (use-package stimmung-themes
;;   :config (load-theme 'stimmung-themes-dark t)
;;   :custom (stimmung-themes-dark-highlight-color "indian red"
;;                                                 "To make highlight red-ish."))

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
