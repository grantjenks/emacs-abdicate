EMACS ?= emacs -Q --batch -L lisp

test:
	$(EMACS) -l lisp/abdicate.el -l test/abdicate-test.el -f ert-run-tests-batch-and-exit

smoke:
	OPENAI_API_KEY=$$OPENAI_API_KEY \
	$(EMACS) --script scripts/run-abdicate.el

bench:
	EMACS_BATCH="emacs -Q -batch -L lisp" ;\
	$$EMACS_BATCH -l lisp/abdicate.el -l lisp/abdicate-bench.el \
	              -f ert-run-tests-batch-and-exit ;\
	echo "Stats:" ;\
	$$EMACS_BATCH --eval "(princ abdicate-bench-stats)"
