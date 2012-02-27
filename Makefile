.PHONY : test
PROLOG=swipl

test:
	$(PROLOG) -g "run_tests,halt." -s dot_dcg.plt