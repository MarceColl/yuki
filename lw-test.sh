#!/bin/sh
# Run the portable core tests on LispWorks in build mode (no IDE).
dir=$(cd "$(dirname "$0")" && pwd)
lw="/Applications/LispWorks 8.1 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-8-1-0-macos64-universal"
cat > "$dir/.lw-test.lisp" <<LISP
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push (pathname "$dir/") asdf:*central-registry*)
(ql:quickload :yuuki/core-test :silent t)
(mp:initialize-multiprocessing "main" nil
  (lambda ()
    (let ((results (fiveam:run :yuuki)))
      (fiveam:explain! results)
      (lw:quit :status (if (fiveam:results-status results) 0 1)))))
LISP
"$lw" -build "$dir/.lw-test.lisp" 2>&1 | grep -vE '^;|^$|^Warning: Lisp compilation'
