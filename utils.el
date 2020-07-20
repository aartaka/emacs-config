(defmacro require-install (package)
  "This macro installs the package if it's not installed already."
    `(if (package-installed-p ,package)
	 (require ,package)
       (progn
	 (package-install ,package)
	 (require ,package))))

(defmacro require-install-many (&rest pkg-list)
  `(progn
     ,@(dolist (pkg pkg-list)
         (require-install pkg))))

;; There is a weird Emacs behavior: it counts ".emacs.d/init.el" file for ".emacs" file.
;; This behavior has reasons behind it, both historical and technical,
;; so I just need to deal with it.
(defun subdir-here (subdir-string)
  "Adds the given dirname to the current directory, place-independently."
  (concat (expand-file-name ".") "/.emacs.d/" subdir-string))

(defun browse-url-icecat (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-firefox-arguments' to Firefox.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new Firefox window.  A non-nil prefix argument
reverses the effect of `browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
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

(defun my-hyperspec-lookup (symbol)
  (interactive (list (common-lisp-hyperspec-read-symbol-name)))
  (let ((browse-url-browser-function 'eww-browse-url))
    (hyperspec-lookup symbol)))

(defun my-hyperspec-lookup-reader-macro (macro)
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Look up reader-macro: "
                       common-lisp-hyperspec--reader-macros nil t
                       (common-lisp-hyperspec-reader-macro-at-point)))))
  (let ((browse-url-browser-function 'eww-browse-url))
    (hyperspec-lookup-reader-macro macro)))

(defun my-hyperspec-lookup-format (character)
  (interactive (list (common-lisp-hyperspec--read-format-character)))
  (let ((browse-url-browser-function 'eww-browse-url))
    (hyperspec-lookup-format character)))

(defun browse-this-file (file)
  (interactive (list (buffer-file-name)))
  (unless (and file (file-exists-p file))
    (error "File does not exist: ‘%s’" file))
  (unless (process-status "httpd")
    (httpd-start))
  (let ((old-httpd-root httpd-root))
    (setf httpd-root (file-name-directory file))
    (browse-url (format "http://127.0.0.1:%s/%s" httpd-port
                        (file-name-nondirectory file)))
    (setq httpd-root old-httpd-root)))

(defun yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
  (interactive)
  (if (require 'helm-config nil t)
      (let ((result (helm-other-buffer
                     (list `((name . ,prompt)
                             (candidates . ,(if display-fn (mapcar display-fn choices)
                                              choices))
                             (action . (("Expand" . identity)))))
                     "*helm-select-yasnippet")))
        (cond ((null result)
               (signal 'quit "user quit!"))
              (display-fn
               (catch 'result
                 (dolist (choice choices)
                   (when (equal (funcall display-fn choice) result)
                     (throw 'result choice)))))
              (t result)))
    nil))
