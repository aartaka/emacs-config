# Emacs customizations

I'm trying to take as much of my necessities and work into Emacs, because it's the best operating system, by coincidence having a decent text editor. And, although it's an [enslaved lisp machine](https://www.reddit.com/r/LispMemes/comments/dgoj02/fun_fact_gnu_emacs_is_mostly_c/), it's, sadly, the best of what we have now.

Nothing that curious about config, though. Happened to define several macros and function, like `require-install` (macro that checks whether the package is already installed and, if it's not, installs it), `require-install-many` (macro that just wraps application of `require-install` to all of its arguments) and `subdir-here` (small helper function that returns the concatenation of the given file with the directory `init.el` file is inside of). Most of the code is either copy-pasted or inspired by the work of others. Some pieces of copypaste even have the links to the original habitat of the borrowed code.

Also, because Guix provides me with the `environment` command, I'm thinking of splitting the config file into several ones -- for coding, academic work and daily tasks, for example.
