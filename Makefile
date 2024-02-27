.POSIX:

EL := init.el early-init.el $(wildcard lisp/*.el)
ELC := $(patsubst %.el,%.elc,$(EL))

all:

clean:
	rm -f $(ELC)

.PHONY: clean
