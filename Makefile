EMACS ?= emacs
EMACSCLIENT ?= emacsclient

# Emacs package directory used by batch commands, including `make test`.
# Override with `make test PACKAGE_USER_DIR=/path/to/elpa` if needed.
PACKAGE_USER_DIR ?= $(HOME)/.emacs.d/elpa

.DEFAULT_GOAL := check

# All project Elisp files, excluding Git internals.
EL_FILES := $(sort $(shell find . -type d -name .git -prune -o \
	-type f -name '*.el' -print))
EL_DIRS := $(sort $(dir $(EL_FILES)))
TEST_FILES := $(filter %-test.el %-tests.el,$(EL_FILES))

EMACS_BATCH = $(EMACS) -Q --batch \
	$(foreach dir,$(EL_DIRS),-L "$(dir)") \
	--eval '(require (quote package))' \
	--eval '(setq package-user-dir "$(PACKAGE_USER_DIR)")' \
	--eval '(package-initialize)' \
	--eval '(setq load-prefer-newer t)'


# These expressions must not contain shell single quotes.
define CHECK_SYNTAX
(let ((files command-line-args-left))
  (setq command-line-args-left nil)
  (dolist (file files)
    (message "Checking %s" file)
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (check-parens))))
endef

# Use absolute paths: the server may have a different working directory.
define INDENT_FILES
(let ((files (quote ($(foreach file,$(EL_FILES),"$(abspath $(file))")))))
  (save-current-buffer
    (dolist (file files)
      (message "Indenting %s" file)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (save-restriction
            (widen)
            (indent-region (point-min) (point-max))))
        (save-buffer))))
  (format "Indented %d files" (length files)))
endef

.PHONY: check lint syntax compile test test-verbose format clean

# Explicit sequencing also keeps "make -j check" safe.
check:
	$(MAKE) clean
	$(MAKE) lint
	$(MAKE) test

clean:
	$(RM) $(EL_FILES:.el=.elc)

compile:
	$(EMACS_BATCH) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(EL_FILES)
format:
	$(EMACS_BATCH) --eval '$(strip $(INDENT_FILES))'

lint:
	$(MAKE) syntax
	$(MAKE) compile

syntax:
	$(EMACS_BATCH) --eval '$(strip $(CHECK_SYNTAX))' $(EL_FILES)

test:
	@$(MAKE) --no-print-directory clean >/dev/null
	@log=$$(mktemp); \
	status=0; \
	ENDE_TEST_LOG="$$log" $(EMACS_BATCH) \
		--eval '(setq auth-sources nil)' \
		$(foreach file,$(TEST_FILES),-l $(file)) \
		-l ./tests/ende-test-runner.el \
		-f ende-test-runner >"$$log" 2>&1 || status=$$?; \
	grep -E '^(Running |TEST-FAILED:|TEST-SUMMARY:|TEST-RUNNER-ERROR:)' "$$log" || true; \
	if [ "$$status" -eq 0 ]; then rm -f "$$log"; \
	else printf 'Full test output: %s\n' "$$log"; fi; \
	exit "$$status"

test-verbose:
	@$(MAKE) --no-print-directory clean >/dev/null
	$(EMACS_BATCH) \
		--eval '(setq auth-sources nil)' \
		$(foreach file,$(TEST_FILES),-l $(file)) \
		-f ert-run-tests-batch-and-exit
