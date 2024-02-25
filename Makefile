.POSIX:

EL := init.el early-init.el $(wildcard lisp/*.el)
ELC := $(patsubst %.el,%.elc,$(EL))

all:
	false

clean:
	rm -f $(ELC)
