checks: tests test-sentence-ends

test-sentence-ends:
	grep -n '[^0-9][.?=)] [A-Z]' *.org *.el; [ $$? = 1 ]

tests:
	emacs --batch -l devil.el -l devil-tests.el -f ert-run-tests-batch-and-exit
