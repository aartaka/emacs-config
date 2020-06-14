#!/bin/bash

mkdir lisp
ln -sf . ~/.emacs.d
curl https://raw.githubusercontent.com/tuhdo/emacs-c-ide-demo/master/custom/setup-helm.el > emacs/lisp/setup-helm.el
curl https://raw.githubusercontent.com/arclanguage/anarki/master/extras/inferior-arc.el > emacs/lisp/inferior-arc.el
curl https://raw.githubusercontent.com/arclanguage/anarki/master/extras/arc.el > emacs/lisp/arc.el
curl https://www.emacswiki.org/emacs/download/column-marker.el > emacs/lisp/column-marker.el
curl https://raw.githubusercontent.com/pmarinov/clean-aindent-mode/master/clean-aindent-mode.el > emacs/lisp/clean-aindent-mode.el
