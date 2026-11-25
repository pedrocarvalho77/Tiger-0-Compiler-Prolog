SWIPL ?= swipl
PASS  = tests/t1.txt tests/t2.txt tests/t3.txt tests/t4.txt tests/test5.txt

.PHONY: run exec test

# make run FILE=tests/t1.txt   (print tokens, AST and intermediate code)
run:
	@cd src && $(SWIPL) -q -g "main_compile('../$(FILE)')" -t halt compiler.pl

# make exec FILE=tests/t2.txt  (compile and execute; reads stdin for scani)
exec:
	@cd src && $(SWIPL) -q -g "main_run('../$(FILE)')" -t halt compiler.pl

# Each file in PASS must compile and print exactly tests/X.out (stdin from
# tests/X.in if it exists). tests/bad_type.txt must be rejected.
test:
	@fail=0; \
	for f in $(PASS); do \
	  b=$${f%.txt}; in=/dev/null; if [ -f $$b.in ]; then in=$$b.in; fi; \
	  if $(MAKE) -s exec FILE=$$f < $$in 2>&1 | cmp -s - $$b.out; then echo "ok    $$f"; else echo "FAIL  $$f"; fail=1; fi; \
	done; \
	if $(MAKE) -s exec FILE=tests/bad_type.txt >/dev/null 2>&1; then echo "FAIL  tests/bad_type.txt (should be rejected)"; fail=1; else echo "ok    tests/bad_type.txt (rejected)"; fi; \
	exit $$fail
