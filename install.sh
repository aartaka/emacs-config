#!/bin/bash

mkdir lisp
curl https://www.emacswiki.org/emacs/download/screenshot.el > lisp/screenshot.el
curl https://raw.githubusercontent.com/tuhdo/emacs-c-ide-demo/master/custom/setup-helm.el > lisp/setup-helm.el
curl https://raw.githubusercontent.com/arclanguage/anarki/master/extras/inferior-arc.el > lisp/inferior-arc.el
curl https://raw.githubusercontent.com/arclanguage/anarki/master/extras/arc.el > lisp/arc.el
