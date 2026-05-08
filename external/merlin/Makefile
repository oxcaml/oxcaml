PARENT := $(abspath ../..)
DUNE_OPTS := --workspace=$(PARENT)/duneconf/boot.ws

# Pull in RUNTIME_DIR from the parent's generated config; the parent's dune
# rules reference it, and they're in scope when we invoke dune against the
# parent's workspace.
include $(PARENT)/Makefile.config

export RUNTIME_DIR

.DEFAULT_GOAL := build

.PHONY: build test test-promote parent-install

parent-install:
	$(MAKE) -C $(PARENT) install

build:
	dune build $(DUNE_OPTS) $(ARGS)

# If the caller didn't point us at an installed compiler, build and install
# the parent ourselves before running the tests.
MERLIN_TEST_OCAML_PATH ?= $(PARENT)/_install
export MERLIN_TEST_OCAML_PATH
ifeq ($(origin MERLIN_TEST_OCAML_PATH),file)
test test-promote: parent-install
else
test test-promote: SKIP_BUILD_MSG := \
  Skipping building the compiler since MERLIN_TEST_OCAML_PATH was set
endif

test-promote: PROMOTE := --auto-promote

test test-promote:
# We want to make sure that dune re-runs the tests in the compiler changes. But
# the _install directory lives outside the dune boot workspace, so dune doesn't
# track whether it changes. So we hash the contents of the directory and make
# the tests, stick the hash in MERLIN_TEST_OCAML_VERSION, and make the tests
# depend on MERLIN_TEST_OCAML_VERSION.
	@if [ -n "$(SKIP_BUILD_MSG)" ]; then echo "$(SKIP_BUILD_MSG)"; fi; \
	version=$$(find "$$MERLIN_TEST_OCAML_PATH" -type f -exec sha256sum {} + \
	    | sort | sha256sum | awk '{print $$1}'); \
	export MERLIN_TEST_OCAML_VERSION="$$version"; \
	if [ -z "$(strip $(ARGS))" ]; then \
	    dune build $(DUNE_OPTS) @runtest $(PROMOTE); \
	else \
	    dune build $(DUNE_OPTS) $(ARGS) $(PROMOTE); \
	fi
