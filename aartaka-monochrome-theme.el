(deftheme aartaka-monochrome
  "Created 2020-09-06.")

(custom-theme-set-variables
 'aartaka-monochrome
 '(package-selected-packages '(ox-gfm wgrep-helm bbdb pretty-sha-path miniedit web-mode yasnippet-classic-snippets yasnippet-snippets yasnippet-lean skewer-less mmm-mode skewer rainbow-mode keyfreq all-the-icons sly nov pdf-tools pdfgrep esup elisp--witness--lisp flymake-racket racket-mode ggtags helm-gtags use-package w3 base16-theme autopair org-mode)))

(custom-theme-set-faces
 'aartaka-monochrome
 '(warning ((t (:foreground "indian red"))))
 '(font-lock-warning-face ((t (:foreground "indian red"))))
 '(secondary-selection ((t (:extend t :background "DarkOliveGreen"))))
 '(font-lock-string-face ((t (:foreground "DarkOliveGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "indian red" :weight bold))))
 '(show-paren-match ((t (:background "DarkOliveGreen"))))
 '(font-lock-doc-face ((t (:inherit nil :foreground "dark gray" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "white" :slant italic :weight semi-bold)))))

(provide-theme 'aartaka-monochrome)
