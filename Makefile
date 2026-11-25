SWIPL ?= swipl
PASS  = tests/t1.txt tests/t2.txt tests/t3.txt tests/t4.txt tests/test5.txt
XFAIL =

.PHONY: run test
# make run FILE=tests/t1.txt
run:
	@cd src && $(SWIPL) -q -g "tiger0_compiler('../$(FILE)')" -t halt compiler.pl

test:
	@fail=0; \
	for f in $(PASS); do \
	  if $(MAKE) -s run FILE=$$f >/dev/null 2>&1; then echo "ok    $$f"; else echo "FAIL  $$f"; fail=1; fi; \
	done; \
	for f in $(XFAIL); do \
	  if $(MAKE) -s run FILE=$$f >/dev/null 2>&1; then echo "XPASS $$f (now works, move it to PASS)"; fail=1; else echo "xfail $$f (known limitation)"; fi; \
	done; \
	exit $$fail
