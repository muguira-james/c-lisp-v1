* introduction

The program shows howto:
* create a package
* handle a queue using an external lisp queue
* perform either a depth or breadth first search

To run this from sbcl:

* cd to the src directory
* sbcl --load learnlisp1.lsp
* At this point you've loaded and compiled the file into the env. You are sitting at a lisp repl prompt.
* (in-package :lrn)
* (main)

The package is called :lrn (from the top of the file). The 'in-package :lrn' statement brings your variables
into view. The function 'main' is the test function.

The Dockerfile now works ! I should use a staged build so I can create the executable lisp file and only load that in the final image.

Docker build -t lrn .
Docker run -it lrn

