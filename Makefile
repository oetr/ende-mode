EMACS ?= emacs

.DEFAULT_GOAL := check

EL_FILES := $(sort $(wildcard *.el))
TEST_FILES := $(sort $(wildcard *-test.el *-tests.el))

EMACS_BATCH = $(EMACS) -Q --batch -L . \
	--eval '(setq load-prefer-newer t)'

.PHONY: check lint syntax compile test format clean

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
		'(mapc (lambda (file) (message "Checking %s" file) (with-temp-buffer (insert-file-contents file) (emacs-lisp-mode) (check-parens))) command-line-args-left)' \
		$(EL_FILES)

compile:
	$(EMACS_BATCH) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(EL_FILES)

test:
	$(EMACS_BATCH) \
		$(foreach file,$(TEST_FILES),-l $(file)) \
		-f ert-run-tests-batch-and-exit

format:
	$(EMACS_BATCH) --eval '(let ((files command-line-args-left)) (setq command-line-args-left nil) (dolist (file files) (message "Loading %s" file) (load-file file)) (dolist (file files) (message "Indenting %s" file) (with-current-buffer (find-file-noselect file) (indent-region (point-min) (point-max)) (save-buffer))))' $(EL_FILES)
	# $(EMACS_BATCH) \ 
	# 	--eval '(require (quote org-element))' \
	# 	--eval '(let ((files command-line-args-left)) (setq command-line-args-left nil) (dolist (file files) (message "Indenting %s" file) (with-current-buffer (find-file-noselect file) (emacs-lisp-mode) (indent-region (point-min) (point-max)) (save-buffer))))' \
	# 	$(EL_FILES)

clean:
	$(RM) $(EL_FILES:.el=.elc)
