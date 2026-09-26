# AGENTS.md

* When generating Emacs Lisp: prioritize byte-code efficiency. Write functions
  that compile to fewer instructions and use direct opcodes rather than
  function calls. Prefer loop macros (`dolist`, `dotimes`), mutation macros
  (`push`, `setf`), and list-building macros (`nreverse`, `cons`) over
  functional equivalents like `mapcar`. This trades some readability for
  direct bytecode operations and better performance.
  The pattern of `push` in a loop followed by `nreverse` is idiomatic and
  efficient (one in-place reversal) when the list has been let-bound (its
  storage is owned locally); treat it as a single unit, not a double-reversal
  inefficiency. Do not use this pattern on pre-existing lists or parameters.
* Verify Emacs Lisp bytecode claims empirically instead of asserting from
  memory:
  - Disassemble a function:
    `emacs -Q --batch -l FILE.el --eval "(progn (disassemble 'FUNC (current-buffer)) (princ (buffer-string)))"`
  - Expand macros in a form:
    `emacs -Q --batch --eval "(macroexpand-all 'FORM)"`
  - To look up docstrings, properties and declarations, prefer reading the
    elisp source files directly, under one of:
    - /usr/local/share/emacs/<version>/lisp
    - /usr/local/share/emacs/<version>/site-lisp
    - /usr/share/emacs/<version>/lisp
    - /usr/share/emacs/<version>/site-lisp
    - `package-user-dir`
    If the symbol's function or variable value is macro generated (its
    docstring is not literal in the source), fall back to evaluating, for a
    function:
    `(princ (documentation 'SYM) standard-output)`
    or for a variable:
    `(princ (documentation-property 'SYM 'variable-documentation) standard-output)`
    and only then to:
    `emacs -Q --batch -l FILE.el --eval "(let ((inhibit-message t)) (describe-symbol 'SYM) (with-current-buffer \"*Help*\" (princ (buffer-string))))`
  Do not assume a symbol is a regular function without checking — it may be a
  macro, a `defsubst` (which inlines), or have a byte-code optimizer. Always
  confirm whether inlining or optimization occurs.
* Compiled Emacs Lisp (`*.elc`, `*.eln`) is never committed to git: it is
  gitignored and the user's Emacs compiles it locally. Do not stage or commit
  compiled files, and do not treat a stale or missing `.elc` as a defect of the
  source — validate the `.el` by compiling it fresh (e.g. to a temporary
  directory) rather than by comparing against any checked-in artefact.
