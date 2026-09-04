(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push (pathname (directory-namestring *load-truename*)) asdf:*central-registry*)
(ql:quickload :yuuki/capi :silent t)
(yuuki-capi:start)
