EMACS ?= emacs

.DEFAULT_GOAL := check

EL_FILES := $(sort $(wildcard *.el))
TEST_FILES := $(sort $(wildcard *-test.el *-tests.el))

EMACS_BATCH = $(EMACS) -Q --batch -L . \
	--eval '(setq load-prefer-newer t)'

.PHONY: check lint syntax compile test clean

# Explicit sequencing also keeps "make -j check" safe.
check:
	$(MAKE) clean
	$(MAKE) lint
	$(MAKE) test

lint:
	$(MAKE) syntax
	$(MAKE) compile

syntax:
	$(EMACS_BATCH) --eval \
		'(dolist (file command-line-args-left) \
		   (message "Checking %s" file) \
		   (with-temp-buffer \
		     (insert-file-contents file) \
		     (emacs-lisp-mode) \
		     (check-parens)))' \
		$(EL_FILES)

compile:
	$(EMACS_BATCH) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(EL_FILES)

test:
	$(EMACS_BATCH) \
		$(foreach file,$(TEST_FILES),-l $(file)) \
		-f ert-run-tests-batch-and-exit

clean:
	$(RM) $(EL_FILES:.el=.elc)
