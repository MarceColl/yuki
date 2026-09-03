SBCL ?= sbcl
ROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
LISP = $(SBCL) --noinform --non-interactive --eval '(push (pathname "$(ROOT)") asdf:*central-registry*)'

.PHONY: all test clean

all: $(ROOT)bin/yuuki.core

$(ROOT)bin/yuuki.core: $(ROOT)yuuki.asd $(ROOT)*.lisp
	$(LISP) --eval '(ql:quickload :yuuki :silent t)' --eval '(yuuki:save-image "$(ROOT)bin/yuuki.core")'

test:
	$(LISP) --eval '(ql:quickload :yuuki/test :silent t)' --eval '(asdf:test-system :yuuki)'

clean:
	rm -f $(ROOT)bin/yuuki.core $(ROOT)bin/yuuki.core.new
