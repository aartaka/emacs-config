;;=============================================================================
;; MELPA CONFIGURATIONS
;;=============================================================================

(require 'package)

(add-to-list 'package-archives
             (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;;===============================================================================
;; OTHER ESSENTIAL CONFIGURATIONS AND HELPERS
;;===============================================================================

(load "~/.emacs.d/utils.el")

;; Configure contribs and non-MELPA packages path
(add-to-list 'load-path (subdir-here "lisp/"))
(let ((default-directory (subdir-here "lisp/")))
  (normal-top-level-add-subdirs-to-load-path))

(require-install-many use-package
                      autopair
                      company
                      company-quickhelp
                      auto-complete
                      golden-ratio
                      better-defaults
                      all-the-icons
                      keyfreq
                      yasnippet-snippets
                      yasnippet-classic-snippets
                      miniedit
                      bbdb
                      pretty-sha-path
                      wgrep-helm)

(setq wgrep-auto-save-buffer t)

;; BBDB Settings
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)
(setf bbdb-mua-auto-update-p t
      bbdb-mua-pop-up nil
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
        ("Reply-to" . "comments-noreply")))

;; YASnippet
(push #'yas/helm-prompt yas-prompt-functions)
(yas-global-mode)

(autopair-global-mode t)
(add-hook 'after-init-hook (lambda ()
			     (show-paren-mode)
			     (global-company-mode)
			     (company-quickhelp-mode)
                 (keyfreq-mode)
                 (keyfreq-autosave-mode)
                 (auto-save-visited-mode)
                 (global-pretty-sha-path-mode)))

(require-install 'helm)
(require 'helm-config)
;; https://raw.githubusercontent.com/tuhdo/emacs-c-ide-demo/master/custom/setup-helm.el
(require 'setup-helm)

;; Misc customizations
(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(setq-default indent-tabs-mode nil      ;use space to indent by default
              tab-width 4)              ;set appearance of a tab that is represented by 4 spaces
(defconst query-replace-highlight t)    ;highlight during query
(defconst search-highlight t)           ;highlight incremental search
(setq ls-lisp-dirs-first t              ;display dirs first in dired
      ecb-tip-of-the-day nil            ;turn off ECB tips
      split-height-threshold 0          ;split windows over horisontal line
      split-width-threshold nil         ;and not opposite
      inhibit-startup-screen t          ;no splash screen
      initial-buffer-choice #'eshell    ;startup on eshell
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      prefer-coding-system 'utf-8       ;prefer UTF-8
      flyspell-issue-message-flag nil   ;don't print per-word messages
      normal-erase-is-backspace t)      ;fix weird backspace
(global-font-lock-mode t)               ;colorize all buffers

(when window-system
  (tool-bar-mode -1)                    ;disable toolbar
  (menu-bar-mode -1)                    ;disable menubar
  (scroll-bar-mode -1))  			    ;disable scrollbar

;; https://www.emacswiki.org/emacs/download/column-marker.el
(require 'column-marker)
(column-number-mode 1)
(line-number-mode 1)

;; Increase DocView mode's docs readability
(add-hook 'doc-view-mode (lambda ()
                           (interactive)
                           (doc-view-fit-page-to-window)))
(setf doc-view-resolution 400
      doc-view-continuous t)

;; Spell-checking hooks
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq ispell-list-command "--list")

;; Install the PDF-tools that mostly supercedes DocView for PDFs
(pdf-tools-install)

;; Unicode support
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Use local Unicode data (possibly newer than upstream)
(when (file-exists-p "~/.emacs.d/UnicodeData.txt")
  (setq describe-char-unicodedata-file "~/.emacs.d/UnicodeData.txt"))

(set-fontset-font t '(#x1f300 . #x1fad0)
                  (when (member "Noto Emoji" (font-family-list)) "Noto Emoji"))

;; Nov-mode for EPUB files
(require-install 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Org-Mode customizations.
(add-hook 'org-mode-hook
          #'(lambda ()
              (local-set-key (kbd "M-q") 'org-fill-paragraph)))
(setf org-startup-with-inline-images t  ;; Inline images
      org-startup-with-latex-preview t  ;; Inline LaTeX preview
      org-agenda-files `(,(concat org-directory "/schedule.org")
                         ,(concat org-directory "/tasks.org")
                         ,(concat org-directory "/notes.org"))
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-start-on-weekday nil
      org-log-done 'note
      org-log-redeadline 'note
      org-hide-leading-stars t
      org-capture-templates
      `(("t" "Todo" entry
         (file+headline ,(concat org-directory "/tasks.org") "Todos")
         "** TODO %?\n  %i\n  %a")
        ("d" "Deadline" entry
         (file+headline ,(concat org-directory "/tasks.org") "Tasks")
         "** TODO %?\n   DEADLINE %^{Task deadline}T\n   %U")
        ("c" "Chaotic schedules" entry
         (file+headline ,(concat org-directory "/tasks.org") "Chaotic")
         "** TODO %?\n  SCHEDULE %^{Date and Time}T\n")))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Magit starting setup
(require-install 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(setf vc-handled-backends nil)

;; EShell
(require 'em-term)
(global-set-key (kbd "<f12>") 'eshell)

;; EasyPG basic config
(require 'epa-file)
(epa-file-enable)

;; Browser
(setq browse-url-browser-function 'browse-url-icecat)

;; Because list-directory makes me mad
(global-set-key (kbd "C-x C-d") 'dired)
;; Because suspend-emacs makes me mad in X
(unbind-key "C-z")

;; Highligh whitespaces
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Set the commenting and uncommenting to the convenient keys
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; The mode to cope with indentations issues
;; https://raw.githubusercontent.com/pmarinov/clean-aindent-mode/master/clean-aindent-mode.el
(require 'clean-aindent-mode)
;; Change the Return key to indent in addition to newlining
(global-set-key (kbd "RET") 'newline-and-indent)
(setf clean-aindent-is-simple-indent t)
;===============================================================================
; LISP CUSTOMIZATIONS, WEEEEEEE!
;===============================================================================

(require-install-many sly sly-asdf sly-quicklisp paredit racket-mode helm-sly)
(setf inferior-lisp-program "sbcl"
      sly-lisp-implementations '((sbcl ("sbcl"))
                                 (ccl ("ccl"))))
(add-hook 'lisp-mode-hook (lambda () (interactive) (sly-editing-mode 1)))

(require 'clhs)
(clhs-setup)

(global-set-key (kbd "C-h -") 'my-hyperspec-lookup)
(global-set-key (kbd "C-h #") 'my-hyperspec-lookup-reader-macro)
(global-set-key (kbd "C-h %") 'my-hyperspec-lookup-format)

(column-marker-create column-marker-code column-marker-code)
(add-hook 'sly-mode-hook
          (lambda ()
            (interactive)
            (column-marker-3 100)
            (company-mode 1)
            (golden-ratio-mode 1)
            (set (make-local-variable lisp-indent-function)
                 'common-lisp-indent-function)))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Small Paredit fix to not place space in between ,@ and the expression it acts on
(defun no-space-between-@-open-paren (endp delimiter)
  (not (and (eql ?\( delimiter)
            (eql ?\@ (char-before (point))))))

(setq paredit-space-for-delimiter-predicates
      '(no-space-between-@-open-paren))

;; Arc customizations
;; https://raw.githubusercontent.com/arclanguage/anarki/master/extras/inferior-arc.el
(require 'inferior-arc)
;; https://raw.githubusercontent.com/arclanguage/anarki/master/extras/arc.el
(require 'arc)
(add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))

;;===============================================================================
;; C/C++ CUSTOMISATIONS
;;==============================================================================

(require-install-many projectile helm-projectile helm-gtags
                      company-c-headers)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(add-to-list 'company-backends 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook (lambda ()
                                (interactive)
                                (hs-minor-mode 1)
                                (column-marker-1 79)))

(setq c-default-style "linux")

(ggtags-mode 1)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (helm-gtags-mode 1))))

(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Dired so you can jump to any tag
    ;; when navigate project tree with Dired
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

;;===============================================================================
;; PYTHON CUSTOMIZATIONS
;;===============================================================================

(require-install-many elpy flycheck py-autopep8 blacken ein)
;; (require 'ein-notebook)
;; (require 'ein-subpackages)
(elpy-enable)

(setq python-shell-interpreter "ipython"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'elpy-mode-hook
          (lambda ()
            (interactive)
            (py-autopep8-enable-on-save)
            (company-mode -1)
            (auto-complete-mode +1)
            (column-marker-1 80)
            (blacken-mode)))

(add-hook 'ein:notebook-python-mode-hook
          (lambda ()
            (interactive)
            (py-autopep8-enable-on-save)
            (company-mode -1)
            (auto-complete-mode)
            (column-marker-1 80)
            (flycheck-mode)
            (blacken-mode)))


;;=============================================================================
;; WEB-DEVELOPMENT CUSTOMIZATIONS
;;=============================================================================
(require-install-many rainbow-mode
                      js2-mode
                      skewer-less
                      web-mode)

(add-hook 'html-mode-hook (lambda ()
                            (interactive)
                            (rainbow-mode)
                            (skewer-html-mode)
                            (httpd-start)))

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)

;;==============================================================================
;; LOOK CUSTOMIZATIONS
;;==============================================================================

(when (member "Hack" (font-family-list))
  (set-frame-font "Hack-17" t t))

(add-hook 'before-make-frame-hook #'(lambda ()
                                      (interactive)
                                      (when (member "Hack" (font-family-list))
                                        (set-frame-font "Hack-17" t t))
                                      (tool-bar-mode -1)
                                      (menu-bar-mode -1)
                                      (scroll-bar-mode -1)))
(require-install-many base16-theme firecode-theme)
(setq base16-theme-256-color-source 'colors)
(load-theme 'firecode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(emacs-lisp-mode-hook (quote (enable-paredit-mode)))
 '(helm-lisp-fuzzy-completion t)
 '(inferior-lisp-program "sbcl")
 '(lisp-mode-hook
   (quote
    ((lambda nil
       (interactive)
       (sly-editing-mode 1))
     #[nil "\300\301\302\303\211$\207"
           [add-hook font-lock-extend-region-functions sly-extend-region-for-font-lock t]
           5]
     enable-paredit-mode)))
 '(package-selected-packages
   (quote
    (firecode-theme wgrep-helm bbdb pretty-sha-path miniedit web-mode yasnippet-classic-snippets yasnippet-snippets yasnippet-lean skewer-less mmm-mode skewer rainbow-mode keyfreq all-the-icons sly nov pdf-tools pdfgrep esup elisp--witness--lisp flymake-racket racket-mode ggtags helm-gtags use-package w3 base16-theme autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
