SHELL = /usr/bin/env bash
ROOTDIR = .
include Makefile.config_if_required
export ARCH

dune = $(opam_exec) $(DUNE)

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
  utils/config.common.ml \
  utils/config.generated.ml \
  compilerlibs/META \
  otherlibs/dynlink/dynlink_cmo_format.mli \
  otherlibs/dynlink/dynlink_cmxs_format.mli \
  otherlibs/dynlink/dynlink_config.ml \
  otherlibs/dynlink/dynlink_platform_intf.mli \
  otherlibs/unix/unix.ml \
  stdlib/META \
  stdlib/runtime.info \
  stdlib/target_runtime.info \
  stdlib/sys.ml \
  runtime/caml/exec.h \
  runtime/caml/m.h \
  runtime/caml/s.h \
  runtime/caml/version.h \
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
# 	cp chamelon/compat/dune.upstream chamelon/compat/dune
# 	$(dune) build $(ws_main) @chamelon/all

.PHONY: boot-minimizer
boot-minimizer:
	cp chamelon/compat/dune.ox chamelon/compat/dune
	$(dune) build $(ws_boot) @chamelon/all

.PHONY: minimizer
minimizer: runtime-stdlib
	cp chamelon/compat/dune.ox chamelon/compat/dune
	$(dune) build $(ws_main) @chamelon/all

.PHONY: hacking-externals
hacking-externals: _build/_bootinstall
	$(dune) build $(ws_boot) $(coverage_dune_flags) -w "extract_externals/extract_externals.exe"


.PHONY: hacking-runtest
hacking-runtest: _build/_bootinstall
	$(dune) build $(ws_boot) $(coverage_dune_flags) -w $(boot_targets) $(runtest_targets)

# Only needed for running the test tools by hand; runtest will take care of
# building them using Dune
.PHONY: test-tools
test-tools: runtime-stdlib
	$(dune) build $(ws_main) @middle_end/flambda2/tests/tools/all

ARCHES=amd64 arm64
.PHONY: check_all_arches
check_all_arches: _build/_bootinstall
	for arch in $(ARCHES); do \
	  ARCH=$$arch $(dune) build $(ws_boot) ocamloptcomp.cma; \
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
	$(dune) promotion apply $(ws_main)

.PHONY: merlin-build
merlin-build:
	$(MAKE) -C external/merlin build

.PHONY: merlin-test
merlin-test:
	$(MAKE) -C external/merlin test

.PHONY: merlin-promote
merlin-promote:
	$(MAKE) -C external/merlin test-promote

# Intermediary library targets

OCAML_COMPILER_LIBS_DIR := $(CURDIR)/_build/ocaml-compiler-libs
PPX_DERIVERS_DIR := $(CURDIR)/_build/ppx-derivers
SEXPLIB0_DIR := $(CURDIR)/_build/sexplib0
STDLIB_SHIMS_DIR := $(CURDIR)/_build/stdlib-shims
PPXLIB_AST_DIR := $(CURDIR)/_build/ppxlib-ast
PPXLIB_DIR := $(CURDIR)/_build/ppxlib
PPXLIB_JANE_DIR := $(CURDIR)/_build/ppxlib-jane
SEQ_DIR := $(CURDIR)/_build/seq
GEN_DIR := $(CURDIR)/_build/gen
SEDLEX_DIR := $(CURDIR)/_build/sedlex
CMDLINER_DIR := $(CURDIR)/_build/cmdliner
MENHIR_DIR := $(CURDIR)/_build/menhir
YOJSON_DIR := $(CURDIR)/_build/yojson

OCAML_COMPILER_LIBS_LIB := $(OCAML_COMPILER_LIBS_DIR)/install/default/lib
PPX_DERIVERS_LIB := $(PPX_DERIVERS_DIR)/install/default/lib
SEXPLIB0_LIB := $(SEXPLIB0_DIR)/install/default/lib
STDLIB_SHIMS_LIB := $(STDLIB_SHIMS_DIR)/install/default/lib
PPXLIB_AST_LIB := $(PPXLIB_AST_DIR)/install/default/lib
PPXLIB_JANE_LIB := $(PPXLIB_JANE_DIR)/install/default/lib
PPXLIB_LIB := $(PPXLIB_DIR)/install/default/lib
SEQ_LIB := $(SEQ_DIR)/install/default/lib
GEN_LIB := $(GEN_DIR)/install/default/lib
SEDLEX_LIB := $(SEDLEX_DIR)/install/default/lib
CMDLINER_LIB := $(CMDLINER_DIR)/install/default/lib
MENHIR_LIB := $(MENHIR_DIR)/install/default/lib
YOJSON_LIB := $(YOJSON_DIR)/install/default/lib

PPXLIB_BASE_OCAMLPATH := $(OCAML_COMPILER_LIBS_LIB):$(PPX_DERIVERS_LIB):$(SEXPLIB0_LIB):$(STDLIB_SHIMS_LIB)
PPXLIB_JANE_OCAMLPATH := $(PPXLIB_BASE_OCAMLPATH):$(PPXLIB_AST_LIB)
PPXLIB_OCAMLPATH := $(PPXLIB_BASE_OCAMLPATH):$(PPXLIB_AST_LIB):$(PPXLIB_JANE_LIB)
SEDLEX_OCAMLPATH := $(PPXLIB_OCAMLPATH):$(PPXLIB_LIB):$(SEQ_LIB):$(GEN_LIB)

OXCAML_INSTALL ?= $(CURDIR)/_install

PPXLIB_DUNE_ENV = \
  PATH="$(OXCAML_INSTALL)/bin:$(PATH)" \
  OCAMLLIB="$(OXCAML_INSTALL)/lib/ocaml" \
  DUNE_CACHE=disabled

.PHONY: external-libs-compiler
external-libs-compiler:
	@mkdir -p "$(CURDIR)/_build"
	@test -x "$(OXCAML_INSTALL)/bin/ocamlc.opt" || $(MAKE) _install

.PHONY: ocaml-compiler-libs-build
ocaml-compiler-libs-build: external-libs-compiler
	env -u OCAMLPATH $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root=external/ocaml-compiler-libs \
	    --build-dir="$(OCAML_COMPILER_LIBS_DIR)" \
	    --only-packages=ocaml-compiler-libs \
	    @install

.PHONY: ppx-derivers-build
ppx-derivers-build: external-libs-compiler
	env -u OCAMLPATH $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root="$(PPXLIB_PPX_DERIVERS_SRC)" \
	    --build-dir="$(PPX_DERIVERS_DIR)" \
	    --only-packages=ppx_derivers \
	    @install

.PHONY: sexplib0-build
sexplib0-build: external-libs-compiler
	env -u OCAMLPATH $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root="$(PPXLIB_SEXPLIB0_SRC)" \
	    --build-dir="$(SEXPLIB0_DIR)" \
	    --only-packages=sexplib0 \
	    @install

.PHONY: stdlib-shims-build
stdlib-shims-build: external-libs-compiler
	env -u OCAMLPATH $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root="$(PPXLIB_STDLIB_SHIMS_SRC)" \
	    --build-dir="$(STDLIB_SHIMS_DIR)" \
	    --only-packages=stdlib-shims \
	    @install

.PHONY: ppxlib-ast-build
ppxlib-ast-build: \
  ocaml-compiler-libs-build ppx-derivers-build sexplib0-build stdlib-shims-build
	env OCAMLPATH="$(PPXLIB_BASE_OCAMLPATH)" $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root=external/ppxlib \
	    --build-dir="$(PPXLIB_AST_DIR)" \
	    --only-packages=ppxlib_ast \
	    @install

.PHONY: ppxlib-jane-build
ppxlib-jane-build: ppxlib-ast-build
	env OCAMLPATH="$(PPXLIB_JANE_OCAMLPATH)" $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root=external/ppxlib_jane \
	    --build-dir="$(PPXLIB_JANE_DIR)" \
	    --only-packages=ppxlib_jane \
	    @install

.PHONY: ppxlib-build
ppxlib-build: ppxlib-jane-build
	env OCAMLPATH="$(PPXLIB_OCAMLPATH)" $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root=external/ppxlib \
	    --build-dir="$(PPXLIB_DIR)" \
	    --only-packages=ppxlib \
	    @install

# The "seq" findlib package is a compatibility shim: Seq has been part of the
# stdlib since OCaml 4.07, so opam only installs an empty META for it. gen
# still lists it as a dependency, so provide the same empty META here.
.PHONY: seq-build
seq-build:
	@mkdir -p "$(SEQ_LIB)/seq"
	@printf '%s\n' \
	  'requires = ""' \
	  'version = "[distributed with OCaml 4.07 or above]"' \
	  > "$(SEQ_LIB)/seq/META"

.PHONY: gen-build
gen-build: external-libs-compiler seq-build
	env OCAMLPATH="$(SEQ_LIB)" $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root="$(SEDLEX_GEN_SRC)" \
	    --build-dir="$(GEN_DIR)" \
	    --only-packages=gen \
	    @install

# --ignore-promoted-rules keeps dune from regenerating the checked-in
# unicode.ml, which would require downloading the Unicode data files.
.PHONY: sedlex-build
sedlex-build: ppxlib-build gen-build
	env OCAMLPATH="$(SEDLEX_OCAMLPATH)" $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root=external/sedlex \
	    --build-dir="$(SEDLEX_DIR)" \
	    --only-packages=sedlex \
	    --ignore-promoted-rules \
	    @install

# Cmdliner ships a Make build, not a Dune project. Copy its read-only source
# into the build tree and build only the library with its upstream Makefile.
.PHONY: cmdliner-build
cmdliner-build: external-libs-compiler
	rm -rf "$(CMDLINER_DIR)/source"
	mkdir -p "$(CMDLINER_DIR)/source"
	cp -R "$(JSOO_CMDLINER_SRC)/." "$(CMDLINER_DIR)/source/"
	chmod -R u+w "$(CMDLINER_DIR)/source"
	env -u OCAMLFIND_TOOLCHAIN OCAMLPATH= $(PPXLIB_DUNE_ENV) \
	  $(MAKE) -C "$(CMDLINER_DIR)/source" \
	    PREFIX="$(CMDLINER_DIR)/install/default" \
	    LIBDIR="$(CMDLINER_LIB)/cmdliner" \
	    build-byte build-native build-native-dynlink \
	    install-common install-srcs install-byte install-native \
	    install-native-dynlink

.PHONY: menhir-libs-build
menhir-libs-build: external-libs-compiler
	env OCAMLPATH= $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root="$(JSOO_MENHIR_SRC)" \
	    --build-dir="$(MENHIR_DIR)" \
	    --only-packages=menhirLib,menhirSdk \
	    @install

.PHONY: yojson-build
yojson-build: external-libs-compiler seq-build
	env OCAMLPATH="$(SEQ_LIB)" $(PPXLIB_DUNE_ENV) \
	  $(dune) build \
	    --root="$(JSOO_YOJSON_SRC)" \
	    --build-dir="$(YOJSON_DIR)" \
	    --only-packages=yojson \
	    @install

.PHONY: external-libs-build
external-libs-build: ppxlib-build sedlex-build

.PHONY: fmt
fmt: $(dune_config_targets)
	$(if $(filter 1,$(V)),,@)bash scripts/fmt.sh


.PHONY: check-fmt
check-fmt: $(dune_config_targets)
	$(if $(filter 1,$(V)),,@)bash tools/ci/actions/check-fmt.sh

.PHONY: regen-flambda2-parser-messages
regen-flambda2-parser-messages: $(dune_config_targets)
	$(dune) build $(ws_boot) @middle_end/flambda2/parser/regen-messages --auto-promote || true

.PHONY: regen-flambda2-tests
regen-flambda2-tests: boot-compiler regen-flambda2-test-dune-rules
	$(dune) build $(ws_runstd) @middle_end/flambda2/tests/regen --auto-promote || true
	$(dune) build $(ws_runstd) @middle_end/flambda2/tests/regen

.PHONY: regen-flambda2-test-dune-rules
regen-flambda2-test-dune-rules: $(dune_config_targets)
	$(dune) build $(ws_boot) @middle_end/flambda2/tests/regen-dune-rules --auto-promote || true
	$(dune) build $(ws_boot) @middle_end/flambda2/tests/regen-dune-rules

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
	$(opam_exec) bisect-ppx-report html --tree -o _coverage \
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
