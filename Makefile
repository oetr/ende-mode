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
TEST_SELECTOR ?= t
TEST_VERBOSE ?= 0
BENCHMARK_SIZES ?= 100 1000 10000
BENCHMARK_SAMPLES ?= 5
BENCHMARK_INSTRUMENT ?= nil

# Libraries shipped by the MELPA recipe; do not lint test fixtures as packages.
PACKAGE_FILES := $(sort $(wildcard lisp/ende*.el))

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

.PHONY: check check-docs check-package check-melpa lint syntax compile test test-fast test-verbose test-compiled test-run format clean benchmark-search benchmark-search-quick benchmark-chat

# Explicit sequencing also keeps "make -j check" safe.
check:
	$(MAKE) clean
	$(MAKE) lint
	$(MAKE) test-run

# Additional submission checks.  Checkdoc reports style warnings (advisory);
# package-lint fails on diagnostics.  Install package-lint in PACKAGE_USER_DIR
# before running check-package or check-melpa; never install dependencies here.
check-docs:
	$(EMACS_BATCH) --eval '(require (quote checkdoc))' \
		--eval '(let ((files command-line-args-left)) (setq command-line-args-left nil) (dolist (file files) (checkdoc-file file)))' $(PACKAGE_FILES)

check-package:
	$(EMACS_BATCH) \
		--eval '(unless (require (quote package-lint) nil t) (error "Install package-lint in PACKAGE_USER_DIR before running check-package"))' \
		--eval '(setq package-lint-main-file (expand-file-name "lisp/ende.el") package-lint-batch-fail-on-warnings t)' \
		-f package-lint-batch-and-exit $(PACKAGE_FILES)

# Keep the ordinary development checks usable without package-lint.
# This is the single entry point for local or CI submission checks.
check-melpa:
	$(MAKE) check
	$(MAKE) check-docs
	$(MAKE) check-package

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
	@$(MAKE) --no-print-directory test-run

test-fast:
	@$(MAKE) --no-print-directory test TEST_SELECTOR='(not (tag integration))'

test-verbose:
	@$(MAKE) --no-print-directory clean >/dev/null
	@$(MAKE) --no-print-directory test-run TEST_VERBOSE=1

test-compiled:
	$(MAKE) clean
	$(MAKE) compile
	$(MAKE) test-run

benchmark-search:
	@$(MAKE) --no-print-directory compile >/dev/null
	@$(EMACS_BATCH) -l ende-search-benchmark \
		--eval '(setq ende-search-benchmark-sizes (quote ($(BENCHMARK_SIZES))) ende-search-benchmark-samples $(BENCHMARK_SAMPLES) ende-search-benchmark-instrument $(BENCHMARK_INSTRUMENT))' \
		-f ende-search-benchmark-run

benchmark-search-quick: BENCHMARK_SIZES = 100
benchmark-search-quick: BENCHMARK_SAMPLES = 1
benchmark-search-quick: benchmark-search

benchmark-chat:
	@$(MAKE) --no-print-directory compile >/dev/null
	@$(EMACS_BATCH) -l ende-chat-benchmark -f ende-chat-benchmark-run

test-run:
	@log=$$(mktemp); \
	status=0; \
	ENDE_TEST_LOG="$$log" $(EMACS_BATCH) \
		--eval '(setq auth-sources nil)' \
		--eval '(setq ende-test-runner--load-start (current-time))' \
		$(foreach file,$(TEST_FILES),-l $(file)) \
		-l ./tests/ende-test-runner.el \
		--eval '(if (= $(TEST_VERBOSE) 1) (ert-run-tests-batch-and-exit (quote $(TEST_SELECTOR))) (ende-test-runner (quote $(TEST_SELECTOR))))' >"$$log" 2>&1 || status=$$?; \
	if [ "$(TEST_VERBOSE)" = 1 ]; then cat "$$log"; \
	else grep -E '^(Running |TEST-FAILED:|TEST-SUMMARY:|TEST-RUNNER-ERROR:|TEST-LOAD:|TEST-SLOW:)' "$$log" || true; fi; \
	if [ "$$status" -eq 0 ]; then rm -f "$$log"; \
	else printf 'Full test output: %s\n' "$$log"; fi; \
	exit "$$status"
