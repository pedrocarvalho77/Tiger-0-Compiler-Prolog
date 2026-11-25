SWIPL ?= swipl
PASS  = tests/t1.txt tests/t2.txt tests/t3.txt tests/t4.txt tests/test5.txt tests/nested.txt
BAD   = tests/bad_type.txt tests/bad_for.txt

MARS  ?=

.PHONY: run exec mips test test-mips

# make mips FILE=tests/t2.txt > t2.s   (print MIPS assembly)
mips:
	@cd src && $(SWIPL) -q -g "main_mips('../$(FILE)')" -t halt compiler.pl

# Like test, but compiles to MIPS and runs the assembly in MARS (java -jar):
#   make test-mips MARS=/path/to/Mars.jar
# The assembly goes to .test.s (ignored by git). Trailing newlines are ignored
# because MARS prints one when the program exits.
test-mips:
	@if [ -z "$(MARS)" ]; then echo "set MARS=/path/to/Mars.jar"; exit 2; fi; \
	fail=0; \
	for f in $(PASS); do \
	  b=$${f%.txt}; in=/dev/null; if [ -f $$b.in ]; then in=$$b.in; fi; \
	  $(MAKE) -s mips FILE=$$f > .test.s 2>/dev/null; \
	  if [ "$$(java -jar $(MARS) nc .test.s < $$in 2>&1)" = "$$(cat $$b.out)" ]; then echo "ok    $$f (MIPS)"; else echo "FAIL  $$f (MIPS)"; fail=1; fi; \
	done; \
	exit $$fail

# make run FILE=tests/t1.txt   (print tokens, AST and intermediate code)
run:
	@cd src && $(SWIPL) -q -g "main_compile('../$(FILE)')" -t halt compiler.pl

# make exec FILE=tests/t2.txt  (compile and execute; reads stdin for scani)
exec:
	@cd src && $(SWIPL) -q -g "main_run('../$(FILE)')" -t halt compiler.pl

# Each file in PASS must compile and print exactly tests/X.out (stdin from
# tests/X.in if it exists). the files in BAD must be rejected.
test:
	@fail=0; \
	for f in $(PASS); do \
	  b=$${f%.txt}; in=/dev/null; if [ -f $$b.in ]; then in=$$b.in; fi; \
	  if $(MAKE) -s exec FILE=$$f < $$in 2>&1 | cmp -s - $$b.out; then echo "ok    $$f"; else echo "FAIL  $$f"; fail=1; fi; \
	done; \
	for f in $(BAD); do \
	  if $(MAKE) -s exec FILE=$$f >/dev/null 2>&1; then echo "FAIL  $$f (should be rejected)"; fail=1; else echo "ok    $$f (rejected)"; fi; \
	done; \
	exit $$fail
