Smap Web Services
=================

This directory contains the implementation of some web services
used by Smap to execute programs for various languages
(Curry, Haskell, Prolog) and systems.

Each service implemented here runs as a CGI script.
This CGI script is intended to be executed by a POST message
having the program to be executed on stdin.

The result of the program execution is returned as plain text
in the following format:

* First line: exit status ("0" for normal execution)
* Following lines: output or error messages
