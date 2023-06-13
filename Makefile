checks: tests test-sentence-ends

test-sentence-ends:
	grep -n '[^0-9]\. [^ lswxz.,"(]' MANUAL.org; [ $$? = 1 ]
	grep -n '\. [^ a]' README.org CHANGES.org LICENSE.org; [ $$? = 1 ]
	grep -n '\. [^ m]' *.el; [ $$? = 1 ]
	grep -n '[?!] [^ ]' *.org *.el; [ $$? = 1 ]

tests:
	emacs --batch -l devil.el -l devil-tests.el -f ert-run-tests-batch-and-exit
