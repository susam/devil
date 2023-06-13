checks: tests test-sentence-ends

test-sentence-ends:
	! grep -n '[^0-9]\. [^ lswxz.,"(]' MANUAL.org
	! grep -n '\. [^ a]' README.org CHANGES.org LICENSE.org
	! grep -n '\. [^ m]' *.el
	! grep -n '[?!] [^ ]' *.org *.el

tests:
	emacs --batch -l devil.el -l devil-tests.el -f ert-run-tests-batch-and-exit
