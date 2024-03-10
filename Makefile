.POSIX:

EL := init.el $(wildcard lisp/*.el)
ELC := $(patsubst %.el,%.elc,$(EL))
PATH_EL := path.el

all:
# $(ELC)

clean:
	rm -f $(ELC)

copy-path:
	@echo "writing path to $(PATH_EL)"
	@echo "(setenv \"PATH\" \""${PATH}"\")\
		(setq exec-path (split-string (getenv \"PATH\") \":\"))" > $(PATH_EL)
# emacs -nw --batch -f batch-byte-compile $(PATH_EL)

init-time:
	emacs -l lisp/print-init-time.el -f kill-emacs 2>&1

%.elc: %.el
	emacs -nw --batch -f batch-byte-compile $^

.PHONY: clean copy-path
