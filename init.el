(require 'package)

(add-to-list 'load-path "~/.config/emacs/lisp/")
(add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp/")
(add-to-list 'load-path "~/.guix-extra-profiles/emacs-profile/emacs-profile/share/emacs/site-lisp/mu4e/")
(add-to-list 'load-path "~/.guix-extra-profiles/emacs-profile/emacs-profile/share/emacs/site-lisp/")

;; (add-to-list 'load-path "~/.config/emacs/stimmung-themes/")
;; (require 'stimmung-themes)
;; (setf stimmung-themes-dark-highlight-color "#330101")
;; (load-file "~/.config/emacs/stimmung-themes/stimmung-themes-dark-theme.el")
;; (load-theme 'stimmung-themes-dark t)

(add-to-list 'custom-theme-load-path "~/.config/emacs/dark-atoll-theme/")
(load-file "~/.config/emacs/dark-atoll-theme/dark-atoll-theme.el")
(require 'dark-atoll)
(load-theme 'dark-atoll t)

(add-to-list 'load-path "~/git/moirai.el/")
(let ((default-directory "~/.config/emacs/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Add Armenian input method.
(load "~/.config/emacs/armenian.el")

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

(require 'rich-minority)

;;==============================================================================
;; OTHER ESSENTIAL CONFIGURATIONS AND HELPERS
;;==============================================================================

(use-package golden-ratio
  :diminish golden-ratio-mode)
(require 'all-the-icons)
(require 'miniedit)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.01)
(require 'auto-complete)

(require 'wordnut)
(define-key global-map (kbd "C-c d") 'wordnut-search)
(require 'synosaurus)
(define-key global-map (kbd "C-c t") 'synosaurus-lookup)

(require 'yasnippet)
(yas-global-mode)
(require 'yasnippet-snippets)
(require 'yasnippet-classic-snippets)
(define-key global-map (kbd "C-c y") 'helm-yas-complete)
(setq helm-yas-space-match-any-greedy t)

(eval-when-compile
  (pinentry-start))

(defun ar/show-trailing-whitespace ()
  (interactive)
  ;; Show unncessary whitespace that can mess up your diff
  (setf show-trailing-whitespace 1))
(defun ar/set-frame-setting ()
  (interactive)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))
(defun ar/browse-url-librewolf (url &optional new-window)
  "See `browse-url-firefox' for reference"
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           "Librewolf" nil "librewolf" (list url))))
(require 'em-term)

;; Misc customizationsn
(fset 'yes-or-no-p 'y-or-n-p)        ;replace y-e-s by y
(defconst query-replace-highlight t) ;highlight during query
(defconst search-highlight t)        ;highlight incremental search

(when window-system (ar/set-frame-setting))

(if (member "IBM Plex Mono" (font-family-list))
    (set-frame-font "IBM Plex Mono 17" t t)
  (set-frame-font "17" t t))

(setq-default indent-tabs-mode nil)
(setq tab-width 4) ; or any other preferred value

(dolist (binding
         `(;; Use TCP sockets.
           (server-use-tcp t)
           ;; TCP port.
           (server-port 8500)
           ;; Integrate system clipboard into Emacs.
           (save-interprogram-paste-before-kill t)
           ;; This beeping sound annoys me so much...
           (visible-bell t)
           (backup-by-copying t)
           ;; For backups to not clutter everything.
           (backup-directory-alist (("." . "~/.saves/")))
           ;; Delete old file backups silently.
           (delete-old-versions t)
           ;; Display dirs first in dired.
           (ls-lisp-dirs-first t)
           ;; Turn off ECB tips.
           (ecb-tip-of-the-day nil)
           ;; Split windows over horisontal line
           (split-height-threshold 0)
           ;; No splash screen.
           (inhibit-startup-screen t)
           ;; Startup on eshell.
           (initial-buffer-choice ,#'eshell)
           (x-select-request-type (UTF8_STRING COMPOUND_TEXT TEXT STRING))
           (prefer-coding-system 'utf-8)
           ;; Don't print per-word messages.
           (flyspell-issue-message-flag nil)
           ;; Fix weird backspace.
           (normal-erase-is-backspace t)
           (browse-url-browser-function ,#'ar/browse-url-librewolf)))
  (set-default (car binding) (cadr binding)))

(cl-loop for (key . function)
         in '(("C-x C-d" . dired)
              ("C-x C-b" . helm-buffers-list)
              ("C-s" . isearch-forward-regexp)
              ("C-r" . isearch-backward-regexp)
              ("<f12>" . eshell)
              ("C-z" . nil)       ; Because suspend-emacs makes me mad in X
              ("C-c w" . whitespace-mode)    ; Highligh whitespaces
              ("C-c ;" . comment-or-uncomment-region))
         do (define-key global-map (kbd key) function))

(dolist (mode '(show-paren-mode
                auto-save-visited-mode
                global-font-lock-mode
                line-number-mode
                column-number-mode
                epa-file-enable
                global-auto-revert-mode))
  (add-hook 'after-init-hook mode))
(add-hook 'prog-mode-hook 'ar/show-trailing-whitespace)
(add-hook 'before-make-frame-hook 'ar/set-frame-setting)

(require 'column-enforce-mode)
(global-column-enforce-mode)

(require 'eldoc)

(require 'flyspell)
(cl-loop for (hook . function)
         in '((markdown-mode-hook . flyspell-mode)
              (text-mode-hook . turn-on-flyspell)
              (prog-mode-hook . flyspell-prog-mode))
         do (add-hook hook function))
;; EmacsWiki said it helps.
(setf ispell-list-command "--list")

;; Need to install pdf-tools for this to work
(pdf-loader-install)

(require 'emms)
(emms-all)
(emms-default-players)

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

(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(require 'transient)
(require 'magit)
(setf vc-handled-backends nil)
(add-to-list 'magit-tag-version-regexp-alist
               '("^[-._+ ]?-pre-release-\\.?$" . -4)
               t 'equal)
(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-x M-g") 'magit-dispatch)

(use-package clean-aindent-mode
  :bind ("RET" . newline-and-indent)
  :custom (clean-aindent-is-simple-indent t))

(use-package keepass-mode)

(require 'sed-mode)
(add-to-list 'auto-mode-alist '("\\.ed\\'" . sed-mode))

;;==============================================================================
;; SOCIAL
;;==============================================================================

;; (require 'mu4e)

(setq user-mail-address    "mail@aartaka.me"
      user-full-name       "Artyom Bologov"
      smtpmail-default-smtp-server "disroot.org"
      smtpmail-smtp-server "disroot.org"
      send-mail-function   'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-debug-info t
      gnus-select-method '(nnimap "disroot.org"))

(setf message-default-mail-headers (concat message-default-mail-headers "Bcc: mail@aartaka.me\n"))
(setf message-signature "Artyom Bologov\nhttps://aartaka.me")

(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))

(add-hook 'message-mode-hook 'my-message-mode-setup)

(require 'bbdb)
(bbdb-initialize 'mu4e 'message 'gnus)
(bbdb-mua-auto-update-init 'mu4e 'message 'gnus)
(setf bbdb-mua-auto-update-p t
      ;; I don't want BBDB to pop up anytime I read emails.
      bbdb-mua-pop-up nil
      ;; Found somewhere on the Internet and altered to work with the
      ;; services I use.
      bbdb-ignore-message-alist
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
        ("Reply-to" . "notifications")))

(require 'erc)
(setq erc-autojoin-channels-alist '((list ".*\.libera.chat" "lisp" "nyxt")))
(use-package erc-colorize)
(use-package erc-image)

(use-package ement)

;;==============================================================================
;; HELM AND FRIENDS
;;==============================================================================

(defun ar/helm-hide-minibuffer-maybe ()
  (interactive)
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun ar/helm-eshell-enable-history ()
  (interactive)
  (define-key eshell-mode-map (kbd "M-l")
              'helm-eshell-history))

(if (version< "26.0.50" emacs-version)
    (eval-when-compile (require 'helm-lib)))
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(require 'helm)
(require 'helm-autoloads)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
;; Rebind tab to do persistent action.
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; Make TAB works in terminal.
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; List actions using C-z.
(define-key helm-map (kbd "C-z")  'helm-select-action)
(setf helm-lisp-fuzzy-completion t
      helm-scroll-amount 4
      helm-ff-search-library-in-sexp t
      helm-split-window-in-side-p t
      helm-echo-input-in-header-line t
      helm-ff-file-name-history-use-recentf t
      helm-move-to-line-cycle-in-source t
      helm-buffer-skip-remote-checking t
      helm-mode-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-org-headings-fontify t
      helm-M-x-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-lisp-fuzzy-completion t
      helm-buffer-skip-remote-checking t
      helm-locate-fuzzy-match t
      helm-display-header-line nil)
(add-hook 'helm-minibuffer-set-up-hook 'ar/helm-hide-minibuffer-maybe)
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)
(add-hook 'eshell-mode-hook 'ar/helm-eshell-enable-history)
(helm-mode 1)

(cl-loop for (key . command)
         in '(("C-x c" . nil)
              ("C-c h" . helm-command-prefix)
              ("M-x" . helm-M-x)
              ("M-y" . helm-show-kill-ring)
              ("C-x b" . helm-buffers-list)
              ("C-x C-f" . helm-find-files)
              ("C-x C-d" . helm-browse-project)
              ("C-c r" . helm-recentf))
         do (define-key global-map (kbd key) command))
(cl-loop for (key . command)
         in '(("<tab>" . nil)
	      ("C-h SPC" . helm-all-mark-rings)
              ("C-c h o" . helm-occur)
              ("C-c h w" . helm-wikipedia-suggest)
              ("C-c h g" . helm-google-suggest)
              ("C-c h x" . helm-register))
         do (define-key helm-command-map (kbd key) command))
(define-key helm-command-map [remap list-buffers] 'helm-buffers-list)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-)
;; list actions using C-z
(define-key helm-map (kbd "C-z") 'helm-select-action)

(require 'projectile)
(projectile-global-mode)

(require 'helm-projectile)
(helm-projectile-on)
(setf projectile-completion-system 'helm
      projectile-indexing-method 'alien)

(require 'helm-ag)
(setf helm-ag-base-command "rg --no-heading"
      helm-ag-fuzzy-match t
      helm-ag-success-exit-status '(0 2)
      helm-ag-insert-at-point 'symbol)
(define-key global-map (kbd "C-M-s") 'helm-do-ag-project-root)

(require 'helm-swoop)
(define-key global-map (kbd "C-c h o") 'helm-swoop)
(define-key global-map (kbd "C-c h s") 'helm-multi-swoop-all)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
(setf
 ;; Save buffer when helm-multi-swoop-edit complete
 helm-multi-swoop-edit-save t
 ;; If this value is t, split window inside the current window
 helm-swoop-split-with-multiple-windows t
 ;; Split direcion. 'split-window-vertically or
 ;; 'split-window-horizontally
 helm-swoop-split-direction 'split-window-vertically
 ;; If nil, you can boost invoke speed in exchange for text color
 helm-swoop-speed-or-color t)

(require 'helm-company)
(define-key company-mode-map (kbd "C-:") 'helm-company)
(define-key company-active-map (kbd "C-:") 'helm-company)

(require 'helm-emms)

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
  (setf (cadr (cl-assoc "beamer" org-latex-classes :test 'equal))
             "\\documentclass[presentation,xcolor=x11names]{beamer}")
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)))

(use-package babel)
(use-package ob-go)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (go . t)
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
   (sqlite . t)))
(setf org-babel-C-compiler "clang")

(require 'ox-gemini)
(add-to-list 'org-export-backends 'gemini)
(require 'ox-pandoc)
(add-to-list 'org-export-backends 'pandoc)
(require 'ox-gfm)
(add-to-list 'org-export-backends 'gfm)
(require 'ox-twbs)
(add-to-list 'org-export-backends 'twbs)

(require 'org-ref)
(define-key org-mode-map (kbd "C-c j") 'org-ref-cite-insert-helm)
(require 'org-ref-url-utils)
(setf reftex-default-bibliography '("~/Documents/bibtex/bibliography.bib")
      org-ref-bibliography-notes "~/Documents/bibtex/notes.org"
      org-ref-default-bibliography reftex-default-bibliography
      org-ref-pdf-directory "~/Documents/bibtex/bibtex-pdfs/"
      bibtex-completion-bibliography '("~/Documents/bibtex/bibliography.bib")
      bibtex-completion-library-path "~/Documents/bibtex/bibtex-pdfs"
      bibtex-completion-notes-path "~/Documents/bibtex/helm-bibtex-notes"
      tex-bibtex-command "biber"
      tex-run-command "pdftex"
      latex-run-command "pdflatex")

(require 'org-make-toc)

(require 'markdown-mode)
;; (require 'gemini-mode)
(require 'edit-indirect)

(require 'writegood-mode)
(add-hook 'text-mode-hook 'writegood-mode)

;;===============================================================================
;; LISP CUSTOMIZATIONS, WEEEEEEE!
;;===============================================================================

(require 'sly)

(defun ar/set-lisp-indent ()
  (interactive)
  (set (make-local-variable lisp-indent-function)
       'common-lisp-indent-function))

(defun ar/set-lisp-columns ()
  (interactive)
  (set-variable 'column-enforce-column 100 t))

(add-hook 'lisp-mode-hook 'ar/set-lisp-columns)
(add-hook 'lisp-mode-hook 'ar/set-lisp-indent)
(add-hook 'sly-editing-mode-hook 'company-mode)

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
                       common-lisp-hyperspec--reader-macros nil t
                       (common-lisp-hyperspec-reader-macro-at-point)))))
  (let ((browse-url-browser-function 'eww-browse-url))
    (hyperspec-lookup-reader-macro macro)))

(defun ar/hyperspec-lookup-format (character)
  (interactive (list (common-lisp-hyperspec--read-format-character)))
  (let ((browse-url-browser-function 'eww-browse-url))
    (hyperspec-lookup-format character)))

(setf sly-lisp-implementations
      `((sbcl ("sbcl" "--dynamic-space-size" "5000") :coding-system utf-8-unix)
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
        (ccl  ("ccl"))
        (gcl ("gcl"))
        (abcl ("abcl"))
        (clisp ("clisp"))
        (clasp ("clasp")))
      inferior-lisp-program "sbcl"
      sly-connection-update-interval 0.1)
(define-key global-map (kbd "C-h -") 'ar/hyperspec-lookup)
(define-key global-map (kbd "C-h #") 'ar/hyperspec-lookup-reader-macro)
(define-key global-map (kbd "C-h ~") 'ar/hyperspec-lookup-format)
(define-key sly-mode-map (kbd "C-c C-j") 'sly-eval-print-last-expression)

(require 'sly-asdf)
(require 'sly-macrostep)
;; (require 'sly-named-readtables)
(require 'sly-quicklisp)
(require 'helm-sly)
(require 'helm-xref)

(require 'racket-mode)
(require 'geiser)
(add-hook 'scheme-mode-hook 'geiser-mode)
(setf geiser-log-verbose t)
(setf geiser-log--max-buffer-size 10000000000)
(require 'geiser-guile)
(setf geiser-guile-binary (list "guile" "--debug" "-L" "/home/aartaka/.guix-profile/share/guile/site/3.0"))

(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-hook 'clojure-mode-hook 'subword-mode)

(require 'parseclj)
(require 'parseedn)

(require 'cider)
(setf cider-repl-pop-to-buffer-on-connect t
      cider-show-error-buffer t
      cider-auto-select-error-buffer t
      cider-repl-history-file "~/.config/emacs/cider-history"
      cider-repl-wrap-history t)
(add-hook 'cider-mode-hook 'eldoc-mode)
(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))
(define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
(define-key clojure-mode-map (kbd "C-c C-r C-r") 'cider-eval-buffer)

(require 'paredit)
(defun no-space-between-@-open-paren (endp delimiter)
  (not (and (eql ?\( delimiter)
            (eql ?\@ (char-before (point))))))
(setf paredit-space-for-delimiter-predicates
      '(no-space-between-@-open-paren))
(dolist (hook '(sly-mode-hook
                sly-editing-mode-hook
                emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
                geiser-mode-hook
                arc-mode-hook
                clojure-mode-hook
                cider-repl-mode))
  (add-hook hook 'paredit-mode))

;; (require 'tagedit)
;; (tagedit-add-paredit-like-keybindings)
;; (add-hook 'html-mode-hook 'tagedit-mode)
;; (add-hook 'web-mode-hook 'tagedit-mode)

;;==============================================================================
;; C/C++ CUSTOMISATIONS
;;==============================================================================

(require 'hideshow)
(setf hs-special-modes-alist
      (append '((csharp-mode "{" "}" "/[*/]" nil nil))
              hs-special-modes-alist)
      hs-isearch-open t)
(define-key hs-minor-mode-map (kbd "C--") 'hs-toggle-hiding)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'hs-minor-mode 'hs-hide-all)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(require 'cc-mode)

(setf (cdr (assoc 'other c-default-style)) "linux"
      c-basic-offset 8
      gdb-many-windows t
      gdb-show-main t)
(define-key c-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
(define-key c++-mode-map (kbd "<tab>") 'company-indent-or-complete-common)

(require 'nhexl-mode)
(setf nhexl-display-unprintables t)

(require 'go-mode)

(use-package brainfuck-mode
  :after hideshow
  :config
  (defun bf-setup ()
    (setf comment-start ";"
          comment-end "")
    (setf (buffer-local-value 'indent-line-function (current-buffer))
          (lambda ()
            (unless (= (point) (point-min))
              (let* ((text-before (cl-subseq (buffer-string) (point-min) (1- (point-at-bol))))
                     (opening (cl-count ?\[ text-before))
                     (old-point (point))
                     (old-indent
                      (progn (beginning-of-line)
                             (skip-chars-forward " \t")
                             (point)))
                     (closing (+ (cl-count ?\] text-before)
                                 (if (= (char-after) ?\])
                                     1 0)))
                     (depth
                      (1+ (- opening closing)))
                     (new-indent (progn
                                   (indent-line-to depth)
                                   (point))))
                (setf (point) (+ old-point (- new-indent old-indent)))))))
    (add-to-list 'hs-special-modes-alist
                 '(brainfuck-mode "\\[" "\\]" ";" nil)))
  :hook
  (brainfuck-mode . hs-minor-mode)
  (brainfuck-mode . bf-setup))

;; :custom
;; (elpy-modules (delq 'elpy-module-flymake elpy-modules))
;; :hook (elpy-mode . flycheck-mode)
(require 'flycheck)
(require 's)
(setf flycheck-disabled-checkers
      (append flycheck-disabled-checkers
              '(javascript-jshint json-jsonlist)))
(add-hook 'after-init-hook 'global-flycheck-mode)
;; (flycheck-add-mode 'javascript-eslint 'web-mode)

(require 'helm-flycheck)

(require 'ein)
(require 'ein-notebook)
(add-hook 'ein:notebook-python-mode-hook 'ar/switch-company-to-ac)
(add-hook 'ein:notebook-python-mode-hook 'flycheck-mode)

;;=============================================================================
;; WEB-DEVELOPMENT CUSTOMIZATIONS
;;=============================================================================

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setf web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
      web-mode-markup-indent-offset 2)

(require 'js2-mode)
;; (add-hook 'web-mode-hook 'js2-minor-mode)
(require 'rjsx-mode)
(use-package skewer-less)
(defun ar/browse-this-file (file)
  (interactive (list (buffer-file-name)))
  (unless (and file (file-exists-p file))
    (error "File does not exist: ‘%s’" file))
  (unless (process-status "httpd")
    (httpd-start))
  (setf httpd-root (file-name-directory file))
  (browse-url (format "http://127.0.0.1:%s/%s" httpd-port
                      (file-name-nondirectory file))))
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode 'skewer-css-mode)
(add-hook 'html-mode 'httpd-start)

(require 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'lisp-mode-hook 'rainbow-mode)
(add-hook 'clojure-mode-hook 'rainbow-mode)

(use-package sqlup-mode
  :hook (sql-interactive-mode . sqlup-mode))

;;==============================================================================
;; LOOK CUSTOMIZATIONS
;;==============================================================================

(require 'smart-mode-line)
(setf sml/theme 'respectful
      sml/no-confirm-load-theme t)
(sml/setup)

(add-to-list 'rm-blacklist " company")
(add-to-list 'rm-blacklist " yas")
(add-to-list 'rm-blacklist " 80col")
(add-to-list 'rm-blacklist " 100col")
(add-to-list 'rm-blacklist " ElDoc")
(add-to-list 'rm-blacklist " Helm")

;; (use-package stimmung-themes
;;   :config (load-theme 'stimmung-themes-dark t)
;;   :custom (stimmung-themes-dark-highlight-color "indian red"
;;                                                 "To make highlight red-ish."))

(setf custom-file "~/.config/emacs/custom.el")
(load custom-file)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
