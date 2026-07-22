PARENT := $(abspath ../..)
DUNE_OPTS := --workspace=$(PARENT)/duneconf/boot.ws

# Pull in RUNTIME_DIR from the parent's generated config; the parent's dune
# rules reference it, and they're in scope when we invoke dune against the
# parent's workspace.
include $(PARENT)/Makefile.config

dune = $(opam_exec) $(DUNE)

export RUNTIME_DIR

.DEFAULT_GOAL := build

.PHONY: build test test-promote parent-install parent-boot-config

parent-boot-config:
	$(MAKE) -C $(PARENT) _build/_bootinstall

<<<<<<< HEAD
parent-install:
	$(MAKE) -C $(PARENT) install
||||||| f61189cc39
bench_disabled:
	merl-an benchmark -p /projects/irmin -s 1 --data=merl-an_bench
	echo "Top 10 slowest queries:"
	jq -s "[ (map(.sample_id |= tostring)) + (map(.sample_id |= tostring)) | group_by(.sample_id)[] | select(length > 1) | add ] | sort_by( .responses[0].timing.clock) | reverse | .[:10]" merl-an_bench/query_responses.json merl-an_bench/commands.json
	echo "Benchmark result:"
	jq . merl-an_bench/bench.json
=======
bench:
	merl-an benchmark -p /projects/irmin,/projects/big-bertha -s 1 --data=merl-an_bench
	echo "Top 10 slowest queries:"
	jq -s "[ (map(.sample_id |= tostring)) + (map(.sample_id |= tostring)) | group_by(.sample_id)[] | select(length > 1) | add ] | sort_by( .responses[0].timing.clock) | reverse | .[:10]" merl-an_bench/query_responses.json merl-an_bench/commands.json
	echo "Benchmark result:"
	jq . merl-an_bench/bench.json
>>>>>>> origin/main

<<<<<<< HEAD
build: parent-boot-config
	$(dune) build $(DUNE_OPTS) $(ARGS)

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

test test-promote: parent-boot-config
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
	    $(dune) build $(DUNE_OPTS) @runtest $(PROMOTE); \
	else \
	    $(dune) build $(DUNE_OPTS) $(ARGS) $(PROMOTE); \
	fi
||||||| f61189cc39
bench:
	echo ""

.PHONY: all build dev clean test promote bench bench_disabled
=======
.PHONY: all build dev clean test promote bench bench
>>>>>>> origin/main
