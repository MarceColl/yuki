SBCL ?= sbcl
LISP = $(SBCL) --noinform --non-interactive --eval '(push (truename ".") asdf:*central-registry*)'

.PHONY: all test clean

all: bin/yuuki.core

bin/yuuki.core: yuuki.asd *.lisp
	$(LISP) --eval '(ql:quickload :yuuki :silent t)' --eval '(yuuki:save-image "bin/yuuki.core")'

test:
	$(LISP) --eval '(ql:quickload :yuuki/test :silent t)' --eval '(asdf:test-system :yuuki)'

clean:
	rm -f bin/yuuki.core bin/yuuki.core.new
