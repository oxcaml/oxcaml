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
  duneconf/ast-dependent-libs.ws \
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
merlin-build: _build/_bootinstall
	$(dune) build $(ws_boot) @external/merlin

.PHONY: merlin-test
merlin-test:
	$(MAKE) -C external/merlin test

.PHONY: merlin-promote
merlin-promote:
	$(MAKE) -C external/merlin test-promote

# AST-dependent libraries: ppxlib, js_of_ocaml and their dependencies
#
# Built with external/ast-dependent-libs as the dune root, against the
# installed compiler in $(OXCAML_INSTALL) (found through PATH and OCAMLLIB).
# That root only holds symlinks to the projects involved, which keeps the
# compiler's own dune rules and $(OXCAML_INSTALL) out of the workspace.
# Sources that are not checked in come from nix (see default.nix) and are
# symlinked into external/ast-dependent-libs/deps/, which is gitignored.

ast_dependent_libs_root = external/ast-dependent-libs
ast_dependent_libs_deps = $(ast_dependent_libs_root)/deps

ws_ast_dependent_libs = --root=$(ast_dependent_libs_root) \
  --workspace=$(CURDIR)/duneconf/ast-dependent-libs.ws \
  --build-dir=$(CURDIR)/_build/ast-dependent-libs
# js_of_ocaml's dune-workspace sets up its test aliases, but is only read by
# default when js_of_ocaml is the dune root.
ws_jsoo_test = --root=$(ast_dependent_libs_root) \
  --workspace=$(CURDIR)/external/js_of_ocaml/dune-workspace \
  --profile=with-effects \
  --build-dir=$(CURDIR)/_build/jsoo-test

define dune_ast_dependent_libs_context
(lang dune 3.23)
(context (default
  (profile release)))
endef

duneconf/ast-dependent-libs.ws: export contents = $(dune_ast_dependent_libs_context)
duneconf/ast-dependent-libs.ws: Makefile

OXCAML_INSTALL ?= $(CURDIR)/_install

ast_dependent_libs_env = \
  env -u OCAMLPATH \
    PATH="$(OXCAML_INSTALL)/bin:$(PATH)" \
    OCAMLLIB="$(OXCAML_INSTALL)/lib/ocaml" \
    OCAMLFIND_CONF=/dev/null \
    DUNE_CACHE=disabled

# The .install files are not promoted, as some of the sources are read-only.
ast_dependent_libs_dune = \
  $(ast_dependent_libs_env) $(dune) build --promote-install-files=false

.PHONY: ast-dependent-libs-compiler
# Refresh the local compiler, but never rebuild an externally supplied install.
ifeq ($(abspath $(OXCAML_INSTALL)),$(CURDIR)/_install)
ast-dependent-libs-compiler: _install
endif
ast-dependent-libs-compiler:
	@test -x "$(OXCAML_INSTALL)/bin/ocamlc.opt"
	@mkdir -p _build

# Against the system compiler, in isolated dune roots so that ppxlib_jane's
# (select ...) picks the upstream shim.
.PHONY: ocaml-compiler-libs-build-boot
ocaml-compiler-libs-build-boot:
	mkdir -p _build
	$(dune) build \
	  --root=external/ocaml-compiler-libs \
	  --build-dir="$(CURDIR)/_build/ocaml-compiler-libs-boot" \
	  @install

.PHONY: ppxlib-jane-build-boot
ppxlib-jane-build-boot:
	mkdir -p _build
	$(dune) build \
	  --root=external/ppxlib_jane \
	  --build-dir="$(CURDIR)/_build/ppxlib-jane-boot" \
	  @default

.PHONY: ast-dependent-libs-build-boot
ast-dependent-libs-build-boot: ocaml-compiler-libs-build-boot ppxlib-jane-build-boot

# Each deps/<name> names the variable holding its nix-provided source.
$(ast_dependent_libs_deps)/ppx_derivers: src_var = PPXLIB_PPX_DERIVERS_SRC
$(ast_dependent_libs_deps)/sexplib0: src_var = PPXLIB_SEXPLIB0_SRC
$(ast_dependent_libs_deps)/stdlib-shims: src_var = PPXLIB_STDLIB_SHIMS_SRC
$(ast_dependent_libs_deps)/gen: src_var = SEDLEX_GEN_SRC
$(ast_dependent_libs_deps)/sedlex: src_var = JSOO_SEDLEX_SRC
$(ast_dependent_libs_deps)/cmdliner: src_var = JSOO_CMDLINER_SRC
$(ast_dependent_libs_deps)/menhir: src_var = JSOO_MENHIR_SRC
$(ast_dependent_libs_deps)/yojson: src_var = JSOO_YOJSON_SRC
$(ast_dependent_libs_deps)/out-channel-redirect: src_var = JSOO_OUT_CHANNEL_REDIRECT_SRC
$(ast_dependent_libs_deps)/qcheck: src_var = JSOO_QCHECK_SRC

PPXLIB_DEPS = \
  $(ast_dependent_libs_deps)/ppx_derivers \
  $(ast_dependent_libs_deps)/sexplib0 \
  $(ast_dependent_libs_deps)/stdlib-shims

JSOO_DEPS = \
  $(ast_dependent_libs_deps)/gen \
  $(ast_dependent_libs_deps)/sedlex \
  $(ast_dependent_libs_deps)/cmdliner \
  $(ast_dependent_libs_deps)/menhir \
  $(ast_dependent_libs_deps)/yojson

JSOO_TEST_DEPS = \
  $(ast_dependent_libs_deps)/out-channel-redirect \
  $(ast_dependent_libs_deps)/qcheck

.PHONY: $(PPXLIB_DEPS) $(JSOO_DEPS) $(JSOO_TEST_DEPS)
$(PPXLIB_DEPS) $(JSOO_DEPS) $(JSOO_TEST_DEPS):
	@if [ -z "$($(src_var))" ]; then \
	  echo "error: $(src_var) is not set; the sources of $(@F) are provided" \
	       "by the nix development shell (see default.nix)" >&2; \
	  exit 1; \
	fi
	@mkdir -p $(@D)
	ln -sfn "$($(src_var))" $@

.PHONY: ppxlib-build
ppxlib-build: ast-dependent-libs-compiler duneconf/ast-dependent-libs.ws $(PPXLIB_DEPS)
	$(ast_dependent_libs_dune) $(ws_ast_dependent_libs) @ppxlib-libs

.PHONY: jsoo-build
jsoo-build: ast-dependent-libs-compiler duneconf/ast-dependent-libs.ws \
  $(PPXLIB_DEPS) $(JSOO_DEPS)
	$(ast_dependent_libs_dune) $(ws_ast_dependent_libs) @jsoo-libs

# The packages built by the ppxlib-libs and jsoo-libs aliases.
PPXLIB_PACKAGES = ocaml-compiler-libs ppx_derivers sexplib0 stdlib-shims \
  ppxlib_ast ppxlib ppxlib_jane
JSOO_PACKAGES = $(PPXLIB_PACKAGES) gen sedlex cmdliner menhirLib menhirSdk \
  yojson js_of_ocaml-compiler wasm_of_ocaml-compiler js_of_ocaml-ppx \
  js_of_ocaml

AST_DEPENDENT_LIBS_PREFIX ?= $(OXCAML_INSTALL)

ast_dependent_libs_install = \
  $(ast_dependent_libs_env) $(dune) install $(ws_ast_dependent_libs) \
    --prefix="$(AST_DEPENDENT_LIBS_PREFIX)" $(1)

.PHONY: ppxlib-install
ppxlib-install: ppxlib-build
	$(call ast_dependent_libs_install,$(PPXLIB_PACKAGES))

.PHONY: jsoo-install
jsoo-install: jsoo-build
	$(call ast_dependent_libs_install,$(JSOO_PACKAGES))

.PHONY: jsoo-test
jsoo-test: ast-dependent-libs-compiler \
  $(PPXLIB_DEPS) $(JSOO_DEPS) $(JSOO_TEST_DEPS)
	PROJECT_ROOT="$(CURDIR)/_build/jsoo-test/default/js_of_ocaml" \
	WASM_OF_OCAML=true \
	  $(ast_dependent_libs_dune) $(ws_jsoo_test) @jsoo-test

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
