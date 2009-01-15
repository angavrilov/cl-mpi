#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'cl-mpi)
#+cmu(ext:quit)
#+sbcl(sb-ext:quit)
