SHELL = /usr/bin/env bash
ROOTDIR = .
include Makefile.config_if_required
export ARCH

boot_ocamlc = main_native.exe
boot_ocamlopt = boot_ocamlopt.exe
boot_ocamlj = boot_ocamlj.exe
boot_ocamlmklib = tools/ocamlmklib.exe
boot_ocamldep = tools/ocamldep.exe
boot_ocamlobjinfo = tools/objinfo.exe
ocamldir = .
toplevels_installed = top opttop

CLEAN_DUNE_WORKSPACES = \
  duneconf/boot.ws \
  duneconf/runtime_stdlib.ws \
  duneconf/main.ws

# These are getting rm -rf'd, so be careful with this.

CLEAN_DIRS = \
  _build \
  _build_upstream \
  _compare \
  _coverage \
  _install \
  _profile \
  _runtest

CLEAN_FILES = \
  $(CLEAN_DUNE_WORKSPACES) \
  duneconf/dirs-to-ignore.inc \
  duneconf/ox-extra.inc \
  dune.runtime_selection \
  otherlibs/dune \
  chamelon/dune \
  natdynlinkops \
  otherlibs/dynlink/natdynlinkops \
  ocamlopt_upstream_flags.sexp \
  ocamlopt_oxcaml_flags.sexp \
  boot_oc_cflags.sexp \
  oc_cflags.sexp \
  oc_cppflags.sexp \
  sharedlib_cflags.sexp \
  .rsync-output \
  .rsync-output-compare \
  ocamlc \
  ocamlopt \
  .ocamldebug

DISTCLEAN_DIRS = \
  $(CLEAN_DIRS) \
  autom4te.cache

DISTCLEAN_FILES = \
  $(CLEAN_FILES) \
  Makefile.build_config \
  Makefile.config \
  config.cache \
  config.log \
  config.status \
  configure \
  configure~ \
  libtool \
  manual/src/version.tex \
  manual/src/html_processing/src/common.ml \
  ocamltest/ocamltest_config.ml \
  ocamltest/ocamltest_unix.ml \
  tools/eventlog_metadata \
  utils/config.common.ml \
  utils/config.generated.ml \
  compilerlibs/META \
  otherlibs/unix/unix.ml \
  stdlib/META \
  stdlib/runtime.info \
  stdlib/target_runtime.info \
  stdlib/sys.ml \
  runtime/caml/exec.h \
  runtime/caml/m.h \
  runtime/caml/s.h \
  runtime/caml/version.h \
  runtime4/caml/exec.h \
  runtime4/caml/m.h \
  runtime4/caml/s.h \
  runtime4/caml/version.h \
  $(wildcard otherlibs/*/META)

ifdef dune
  CLEAN_DUNE_BIN := $(dune)
else
  CLEAN_DUNE_BIN := $(shell command -v dune 2>/dev/null)
endif

.PHONY: clean
clean:
	$(if $(filter 1,$(V)),,@)set -eu; \
	  dirs="$(CLEAN_DIRS)"; \
	  if [ -z "$$dirs" ]; then echo "Refusing to clean empty directory list" >&2; exit 1; fi; \
	  for dir in $$dirs; do \
	    case "$$dir" in ""|"/"|".") echo "Refusing to clean $$dir" >&2; exit 1;; esac; \
	  done; \
	  ws_list="$(CLEAN_DUNE_WORKSPACES)"; \
	  if [ -n "$(strip $(CLEAN_DUNE_BIN))" ]; then \
	    for ws in $$ws_list; do \
	      if [ -f $$ws ]; then \
	        if ! "$(strip $(CLEAN_DUNE_BIN))" clean --root=. --workspace=$$ws; then \
	          echo "dune clean failed for workspace $$ws, continuing with manual cleanup" >&2; \
	        fi; \
	      fi; \
	    done; \
	  fi; \
	  rm -rf -- $$dirs; \
	  rm -f -- $(CLEAN_FILES)

.PHONY: distclean
distclean: clean
	$(if $(filter 1,$(V)),,@)set -eu; \
	  dirs="$(DISTCLEAN_DIRS)"; \
	  if [ -z "$$dirs" ]; then echo "Refusing to distclean empty directory list" >&2; exit 1; fi; \
	  for dir in $$dirs; do \
	    case "$$dir" in ""|"/"|".") echo "Refusing to distclean $$dir" >&2; exit 1;; esac; \
	  done; \
	  rm -rf -- $$dirs; \
	  rm -f -- $(DISTCLEAN_FILES)

$(ocamldir)/duneconf/ox-extra.inc:
	echo > $@

include Makefile.common-ox

.PHONY: ci
ifeq ($(coverage),yes)
ci: ci-coverage
else
ci: ci-no-coverage
endif

.PHONY: ci-no-coverage
ci-no-coverage: runtest runtest-upstream minimizer

.PHONY: ci-coverage
ci-coverage: boot-runtest coverage

# CR mshinwell: build is broken
# .PHONY: minimizer-upstream
# minimizer-upstream:
# 	cp chamelon/dune.upstream chamelon/dune
# 	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_main) @chamelon/all

.PHONY: boot-minimizer
boot-minimizer:
	cp chamelon/dune.ox chamelon/dune
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) @chamelon/all

.PHONY: minimizer
minimizer: runtime-stdlib
	cp chamelon/dune.ox chamelon/dune
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_main) @chamelon/all

.PHONY: hacking-externals
hacking-externals: _build/_bootinstall
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) $(coverage_dune_flags) -w "extract_externals/extract_externals.exe"


.PHONY: hacking-runtest
hacking-runtest: _build/_bootinstall
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) $(coverage_dune_flags) -w $(boot_targets) @runtest

# Only needed for running the test tools by hand; runtest will take care of
# building them using Dune
.PHONY: test-tools
test-tools: runtime-stdlib
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_main) @middle_end/flambda2/tests/tools/all

ARCHES=amd64 arm64
.PHONY: check_all_arches
check_all_arches: _build/_bootinstall
	for arch in $(ARCHES); do \
	  ARCH=$$arch RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) ocamloptcomp.cma; \
	done

# Compare the OxCaml installation tree against the upstream one.

.PHONY: compare
compare: _compare/config.status _install
	rm -f .rsync-output-compare
	rsync -i -a --filter=':- $$(pwd)/ocaml/.gitignore' \
	  $$(pwd)/ocaml/ $$(pwd)/_compare \
	  | grep -v '/$$' \
	  | tee .rsync-output-compare
	if [ -s .rsync-output-compare ] || ! [ -d _compare/_install ]; then \
	  (cd _compare && \
	    $(MAKE) world.opt && \
	    $(MAKE) ocamlnat && \
	    $(MAKE) install); \
	fi
	./scripts/compare.sh $$(pwd)/_compare/_install $$(pwd)/_install \
	  _install/bin/ocamlobjinfo.opt

_compare/config.status: ocaml/config.status
	set -eu; rm -rf _compare
	mkdir _compare
	rsync -a --filter=':- $$(pwd)/ocaml/.gitignore' \
	  $$(pwd)/ocaml/ $$(pwd)/_compare
	(cd _compare && ./configure $(CONFIGURE_ARGS) --prefix=$$(pwd)/_install)

.PHONY: promote
promote:
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) promote $(ws_main)

.PHONY: fmt
fmt: $(dune_config_targets)
	$(if $(filter 1,$(V)),,@)bash scripts/fmt.sh


.PHONY: check-fmt
check-fmt: $(dune_config_targets)
	$(if $(filter 1,$(V)),,@)bash tools/ci/actions/check-fmt.sh

.PHONY: regen-flambda2-parser
regen-flambda2-parser: $(dune_config_targets)
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) @middle_end/flambda2/parser/regen --auto-promote || true
# Make sure regeneration is idempotent, and also check that the previous step
# worked (can't tell the difference between failure and successful
# auto-promotion)
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) @middle_end/flambda2/parser/regen

.PHONY: regen-flambda2-tests
regen-flambda2-tests: boot-compiler regen-flambda2-test-dune-rules
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_runstd) \
	  @middle_end/flambda2/tests/regen --auto-promote || true
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_runstd) \
	  @middle_end/flambda2/tests/regen

.PHONY: regen-flambda2-test-dune-rules
regen-flambda2-test-dune-rules: $(dune_config_targets)
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) \
	  @middle_end/flambda2/tests/regen-dune-rules --auto-promote || true
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) \
	  @middle_end/flambda2/tests/regen-dune-rules

## Build upstream compiler.
.PHONY: build_upstream
build_upstream: ocaml/config.status
	rsync -a ocaml/ _build_upstream
	(cd _build_upstream && \
	    $(MAKE) world.opt && \
	    $(MAKE) ocamlnat)

.PHONY: install_upstream
install_upstream: build_upstream
	(cd _build_upstream && $(MAKE) install)
	cp ocaml/VERSION $(prefix)/lib/ocaml/
	ln -s ocamltoplevel.cmxa \
	  $(prefix)/lib/ocaml/compiler-libs/ocamlopttoplevel.cmxa
	ln -s ocamltoplevel.a \
	  $(prefix)/lib/ocaml/compiler-libs/ocamlopttoplevel.a

.PHONY: build_and_test_upstream
build_and_test_upstream: build_upstream
	if $$(which gfortran > /dev/null 2>&1); then \
	  export LIBRARY_PATH=$$(dirname $$(gfortran -print-file-name=libgfortran.a)); \
	fi; \
	export OCAMLSRCDIR=$$(pwd)/_build_upstream \
         && cd _build_upstream/testsuite \
	 && if $$(which parallel > /dev/null 2>&1); \
            then \
	      echo "Running testsuite in parallel (nproc=$$(nproc))"; \
	      make --no-print-directory parallel; \
            else \
	      echo "Running testsuite sequentially"; \
              make --no-print-directory all; \
            fi
	cd _build_upstream && $(MAKE) check_all_arches

.PHONY: coverage
coverage: boot-runtest
	set -eu; rm -rf _coverage
	bisect-ppx-report html --tree -o _coverage \
	  --coverage-path=_build/default \
		--source-path=. \
	  --source-path=_build/default
	@echo Coverage report generated in _coverage/index.html

.PHONY: debug
.NOTPARALLEL: debug
debug: install debug-printers ocamlc ocamlopt .ocamldebug

ocamlc:
	ln -s $(prefix)/bin/ocamlc.byte ocamlc

ocamlopt:
	ln  -s $(prefix)/bin/ocamlopt.byte ocamlopt

.ocamldebug: install
	find _build/main -name '*.cmo' -type f -exec dirname {} \; | sort -u | sed 's/^/directory /' > .ocamldebug
	echo "source _build/main/$(ocamldir)/tools/debug_printers" >> .ocamldebug
