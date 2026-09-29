EMACS ?= emacs
EMACSCLIENT ?= emacsclient

.DEFAULT_GOAL := check

# All project Elisp files, excluding Git internals.
EL_FILES := $(sort $(shell find . -type d -name .git -prune -o \
	-type f -name '*.el' -print))
EL_DIRS := $(sort $(dir $(EL_FILES)))
TEST_FILES := $(filter %-test.el %-tests.el,$(EL_FILES))

EMACS_BATCH = $(EMACS) -Q --batch \
	$(foreach dir,$(EL_DIRS),-L "$(dir)") \
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

.PHONY: check lint syntax compile test format clean

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
	$(EMACS_BATCH) \
		$(foreach file,$(TEST_FILES),-l $(file)) \
		-f ert-run-tests-batch-and-exit
