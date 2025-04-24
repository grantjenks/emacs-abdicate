# Locate emacs on PATH or fall back to the Mac App bundle binary
EMACS ?= $(shell command -v emacs 2>/dev/null || echo /Applications/Emacs.app/Contents/MacOS/Emacs) -Q --batch -L lisp
.PHONY: test smoke bench

test:
	$(EMACS) -l lisp/abdicate.el -l test/abdicate-test.el -f ert-run-tests-batch-and-exit

smoke:
	OPENAI_API_KEY=$$OPENAI_API_KEY \
	$(EMACS) --script scripts/run-abdicate.el

bench:
	# Run benchmarks (using the same EMACS invocation) and then print stats
	$(EMACS) -l lisp/abdicate.el -l lisp/abdicate-bench.el \
		-f ert-run-tests-batch-and-exit ;\
	echo "Stats:" ;\
	$(EMACS) --eval "(princ abdicate-bench-stats)"
