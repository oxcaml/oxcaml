PARENT := $(abspath ../..)
DUNE_OPTS := --workspace=$(PARENT)/duneconf/boot.ws

# Pull in RUNTIME_DIR from the parent's generated config; the parent's dune
# rules reference it, and they're in scope when we invoke dune against the
# parent's workspace.
include $(PARENT)/Makefile.config

MERLIN_TEST_OCAML_PATH ?= $(PARENT)/_install

export RUNTIME_DIR
export MERLIN_TEST_OCAML_PATH

.DEFAULT_GOAL := build

.PHONY: build test parent-install

parent-install:
	$(MAKE) -C $(PARENT) install

build:
	dune build $(DUNE_OPTS) $(ARGS)

test: parent-install
# We want to make sure that dune re-runs the tests in the compiler changes. But
# the _install directory lives outside the dune boot workspace, so dune doesn't
# track whether it changes. So we hash the contents of the directory and make
# the tests, stick the hash in MERLIN_TEST_OCAML_VERSION, and make the tests
# depend on MERLIN_TEST_OCAML_VERSION. This seems to sometimes cause us to
# unnecessarily re-run the tests, but it's better than not re-running them when
# we should.
	@version=$$(find "$$MERLIN_TEST_OCAML_PATH" -type f -exec sha256sum {} + \
	    | sort | sha256sum | awk '{print $$1}'); \
	export MERLIN_TEST_OCAML_VERSION="$$version"; \
	if [ -z "$(strip $(ARGS))" ]; then \
	    dune runtest $(DUNE_OPTS); \
	else \
	    dune build $(DUNE_OPTS) $(ARGS); \
	fi
