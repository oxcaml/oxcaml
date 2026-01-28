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
  dune.runtime_selection \
  otherlibs/dune \
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
<<<<<<< HEAD
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
||||||| upstream
clean::
	$(MAKE) -C testsuite clean

# Build the manual latex files from the etex source files
# (see manual/README.md)
.PHONY: manual-pregen
manual-pregen: opt.opt
	cd manual; $(MAKE) clean && $(MAKE) pregen-etex

clean::
	$(MAKE) -C manual clean

# The clean target
clean:: partialclean
	rm -f configure~
	rm -f $(C_PROGRAMS) $(C_PROGRAMS:=.exe)
	rm -f $(OCAML_PROGRAMS) $(OCAML_PROGRAMS:=.exe)
	rm -f $(OCAML_PROGRAMS:=.opt) $(OCAML_PROGRAMS:=.opt.exe)
	rm -f $(OCAML_BYTECODE_PROGRAMS) $(OCAML_BYTECODE_PROGRAMS:=.exe)
	rm -f $(OCAML_NATIVE_PROGRAMS) $(OCAML_NATIVE_PROGRAMS:=.exe)

# The bytecode compiler

ocamlc_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)

ocamlc_SOURCES = driver/main.mli driver/main.ml

ocamlc_BYTECODE_LINKFLAGS = -compat-32 -g

ifeq "$(IN_COREBOOT_CYCLE)" "true"
ocamlc_BYTECODE_LINKFLAGS += -set-runtime-default standard_library_default=.
endif

partialclean::
	rm -f ocamlc ocamlc.exe ocamlc.opt ocamlc.opt.exe ocamlc*.stripped

# The native-code compiler

ocamlopt_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamloptcomp)

ocamlopt_SOURCES = driver/optmain.mli driver/optmain.ml

ocamlopt_BYTECODE_LINKFLAGS = -g

partialclean::
	rm -f ocamlopt ocamlopt.exe ocamlopt.opt ocamlopt.opt.exe ocamlopt*.stripped

# The toplevel

# At the moment, the toplevel can't be built with the general build macros
# because its build involves calling expunge. We thus give its build
# rules explicitly until the day expunge can hopefully be removed.

ocaml_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp ocamltoplevel)

ocaml_CMA_FILES = $(ocaml_LIBRARIES:=.cma)

ocaml_SOURCES = toplevel/topstart.mli toplevel/topstart.ml

ocaml_CMO_FILES = toplevel/topstart.cmo

.INTERMEDIATE: ocaml.tmp
ocaml.tmp: OC_BYTECODE_LINKFLAGS += -I toplevel/byte -linkall -g
ocaml.tmp: $(ocaml_CMA_FILES) $(ocaml_CMO_FILES)
	$(V_LINKC)$(LINK_BYTECODE_PROGRAM) -o $@ $^

$(eval $(call PROGRAM_SYNONYM,ocaml))
ocaml$(EXE): $(expunge) ocaml.tmp
	- $(V_GEN)$(OCAMLRUN) $^ $@ $(PERVASIVES)

partialclean::
	rm -f ocaml ocaml.exe

# Use TOPFLAGS to pass additional flags to the bytecode or native toplevel
# when running make runtop or make natruntop
TOPFLAGS ?=
OC_TOPFLAGS = $(STDLIBFLAGS) -I toplevel -noinit $(TOPINCLUDES) $(TOPFLAGS)

# Use runtime/ocamlrun rather than boot/ocamlrun since boot/ocamlrun is compiled
# without shared library support on Windows (when bootstrapping flexdll)
RUN_OCAML = $(RLWRAP) $(NEW_OCAMLRUN) ./ocaml$(EXE) $(OC_TOPFLAGS)
RUN_OCAMLNAT = $(RLWRAP) ./ocamlnat$(EXE) $(OC_TOPFLAGS)

# Note: Beware that, since these rules begin with a coldstart, boot/ocamlc must
# produce code capable of being executed using runtime/ocamlrun (i.e. there are
# circumstances where it may be necessary to bootstrap first, but if you're
# doing work which needs it, you probably know that already).
.PHONY: runtop
runtop: coldstart
	$(MAKE) ocamlc
	$(MAKE) ocaml
	@$(RUN_OCAML)

.PHONY: runtop-with-otherlibs
runtop-with-otherlibs: coldstart
	$(MAKE) ocamlc
	$(MAKE) otherlibraries
	$(MAKE) ocaml
	@$(RUN_OCAML)

.PHONY: natruntop
natruntop:
	$(MAKE) core
	$(MAKE) opt
	$(MAKE) ocamlnat
	@$(RUN_OCAMLNAT)

# The dynlink library

dynlink_SOURCES = $(addprefix otherlibs/dynlink/,\
  dynlink_config.mli dynlink_config.ml \
  dynlink_types.mli dynlink_types.ml \
  dynlink_platform_intf.mli dynlink_platform_intf.ml \
  dynlink_common.mli dynlink_common.ml \
  byte/dynlink_symtable.mli byte/dynlink_symtable.ml \
  byte/dynlink.mli byte/dynlink.ml \
  native/dynlink.mli native/dynlink.ml)

dynlink_LIBRARIES =

otherlibs/dynlink/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)
otherlibs/dynlink/%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)


otherlibs/dynlink/%/dynlink.cmi: \
  otherlibs/dynlink/dynlink.cmi otherlibs/dynlink/dynlink.mli
	cp $^ otherlibs/dynlink/$*/

.PHONY: dynlink-all
dynlink-all: otherlibs/dynlink/dynlink.cma

.PHONY: dynlink-allopt
dynlink-allopt: otherlibs/dynlink/dynlink.cmxa

otherlibs/dynlink/dynlink.cma: VPATH += otherlibs/dynlink/byte
otherlibs/dynlink/dynlink.cmxa: VPATH += otherlibs/dynlink/native

ifeq "$(FLAMBDA)" "true"
otherlibs/dynlink/%: OC_NATIVE_COMPFLAGS += -O3
endif

# dynlink.cmx needs to be available in the search path (since
# it is not compiled with -opaque), and we prefer to make the file
# available in a directory that is already searched rather than have
# to add otherlibs/dynlink/native to the search path as well

otherlibs/dynlink/dynlink.cmx : otherlibs/dynlink/native/dynlink.cmx
	cd otherlibs/dynlink; $(LN) native/dynlink.cmx .

DYNLINK_DEPEND_DUMMY_FILES = \
  otherlibs/dynlink/dynlink.ml \
  otherlibs/dynlink/byte/dynlink.mli \
  otherlibs/dynlink/native/dynlink.mli

beforedepend::
	@touch $(DYNLINK_DEPEND_DUMMY_FILES)

otherlibs/dynlink.depend: beforedepend
	@$(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I otherlibs/dynlink $(INCLUDES) \
	  $(OCAMLDEPFLAGS) \
	  -I otherlibs/dynlink/byte \
	  -bytecode otherlibs/dynlink/*.mli otherlibs/dynlink/dynlink_*.ml \
	  otherlibs/dynlink/byte/*.mli otherlibs/dynlink/byte/*.ml \
	  > $@
	@$(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I otherlibs/dynlink $(INCLUDES) \
	  $(OCAMLDEPFLAGS) \
	  -I otherlibs/dynlink/native \
	  -native otherlibs/dynlink/dynlink_*.ml \
	  otherlibs/dynlink/native/dynlink.ml \
	  >> $@

# Cleanup the lexers

partialclean::
	rm -f bytecomp/byterntm.ml parsing/lexer.ml

beforedepend:: bytecomp/byterntm.ml parsing/lexer.ml

# The predefined exceptions and primitives

lambda/runtimedef.ml: lambda/generate_runtimedef.sh runtime/caml/fail.h \
    runtime/primitives
	$(V_GEN)$^ > $@

partialclean::
	rm -f lambda/runtimedef.ml

beforedepend:: lambda/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.mli: asmcomp/$(ARCH)/arch.mli
	@cd asmcomp; $(LN) $(ARCH)/arch.mli .

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	@cd asmcomp; $(LN) $(ARCH)/arch.ml .

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	@cd asmcomp; $(LN) $(ARCH)/proc.ml .

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	@cd asmcomp; $(LN) $(ARCH)/selection.ml .

asmcomp/CSE.ml: asmcomp/$(ARCH)/CSE.ml
	@cd asmcomp; $(LN) $(ARCH)/CSE.ml .

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	@cd asmcomp; $(LN) $(ARCH)/reload.ml .

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	@cd asmcomp; $(LN) $(ARCH)/scheduling.ml .

asmcomp/stackframe.ml: asmcomp/$(ARCH)/stackframe.ml
	@cd asmcomp; $(LN) $(ARCH)/stackframe.ml .

# Preprocess the code emitters
cvt_emit = tools/cvt_emit$(EXE)

beforedepend:: tools/cvt_emit.ml

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp $(cvt_emit)
	$(V_GEN)echo \# 1 \"asmcomp/$(ARCH)/emit.mlp\" > $@ && \
	$(OCAMLRUN) $(cvt_emit) < $< >> $@ \
	|| { rm -f $@; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml tools/cvt_emit.ml

beforedepend:: asmcomp/emit.ml

cvt_emit_LIBRARIES =
cvt_emit_SOURCES = tools/cvt_emit.mli tools/cvt_emit.mll

# The "expunge" utility

expunge_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)

expunge_SOURCES = toplevel/expunge.mli toplevel/expunge.ml

partialclean::
	rm -f expunge expunge.exe

# The runtime system

## Lists of source files

runtime_COMMON_C_SOURCES = \
  addrmap \
  afl \
  alloc \
  array \
  backtrace \
  bigarray \
  blake2 \
  callback \
  codefrag \
  compare \
  custom \
  debugger \
  domain \
  dynlink \
  extern \
  fail \
  fiber \
  finalise \
  floats \
  gc_ctrl \
  gc_stats \
  globroots \
  hash \
  intern \
  ints \
  io \
  lexing \
  lf_skiplist \
  main \
  major_gc \
  md5 \
  memory \
  memprof \
  meta \
  minor_gc \
  misc \
  obj \
  parsing \
  platform \
  printexc \
  prng \
  roots \
  runtime_events \
  shared_heap \
  signals \
  skiplist \
  startup_aux \
  str \
  sync \
  sys \
  $(UNIX_OR_WIN32) \
  weak

runtime_BYTECODE_ONLY_C_SOURCES = \
  backtrace_byt \
  fail_byt \
  fix_code \
  interp \
  startup_byt \
  zstd
runtime_BYTECODE_C_SOURCES = \
  $(runtime_COMMON_C_SOURCES:%=runtime/%.c) \
  $(runtime_BYTECODE_ONLY_C_SOURCES:%=runtime/%.c)

runtime_NATIVE_ONLY_C_SOURCES = \
  $(TSAN_NATIVE_RUNTIME_C_SOURCES) \
  backtrace_nat \
  clambda_checks \
  dynlink_nat \
  fail_nat \
  frame_descriptors \
  startup_nat \
  signals_nat
runtime_NATIVE_C_SOURCES = \
  $(runtime_COMMON_C_SOURCES:%=runtime/%.c) \
  $(runtime_NATIVE_ONLY_C_SOURCES:%=runtime/%.c)

## Header files generated by configure
runtime_CONFIGURED_HEADERS = \
  $(addprefix runtime/caml/, exec.h m.h s.h version.h)

## Header files generated by make
runtime_BUILT_HEADERS = runtime/build_config.h

## Targets to build and install

runtime_PROGRAMS = ocamlrun
runtime_BYTECODE_STATIC_LIBRARIES = runtime/libcamlrun.$(A)
runtime_BYTECODE_SHARED_LIBRARIES =
runtime_NATIVE_STATIC_LIBRARIES = \
  runtime/libasmrun.$(A) runtime/libcomprmarsh.$(A)
runtime_NATIVE_SHARED_LIBRARIES =

ifeq "$(RUNTIMED)" "true"
runtime_PROGRAMS += ocamlrund
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlrund.$(A)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmrund.$(A)
endif

ifeq "$(INSTRUMENTED_RUNTIME)" "true"
runtime_PROGRAMS += ocamlruni
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlruni.$(A)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmruni.$(A)
endif

ifeq "$(UNIX_OR_WIN32)" "unix"
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlrun_pic.$(A)
runtime_BYTECODE_SHARED_LIBRARIES += camlrun
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmrun_pic.$(A)
runtime_NATIVE_SHARED_LIBRARIES += asmrun
endif
endif

## List of object files for each target


libcamlrun_OBJECTS = \
  $(runtime_BYTECODE_C_SOURCES:.c=.b.$(O))

libcamlrun_non_shared_OBJECTS = \
  $(subst $(UNIX_OR_WIN32).b.$(O),$(UNIX_OR_WIN32)_non_shared.b.$(O), \
          $(libcamlrun_OBJECTS))

libcamlrund_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.bd.$(O)) \
  runtime/instrtrace.bd.$(O)

libcamlruni_OBJECTS = \
  $(runtime_BYTECODE_C_SOURCES:.c=.bi.$(O))

libcamlrunpic_OBJECTS = \
  $(runtime_BYTECODE_C_SOURCES:.c=.bpic.$(O))

libasmrun_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.n.$(O)) $(runtime_ASM_OBJECTS)

libasmrund_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.nd.$(O)) $(runtime_ASM_OBJECTS:.$(O)=.d.$(O))

libasmruni_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.ni.$(O)) $(runtime_ASM_OBJECTS:.$(O)=.i.$(O))

libasmrunpic_OBJECTS = $(runtime_NATIVE_C_SOURCES:.c=.npic.$(O)) \
  $(runtime_ASM_OBJECTS:.$(O)=_libasmrunpic.$(O))

libcomprmarsh_OBJECTS = runtime/zstd.npic.$(O)

## General (non target-specific) assembler and compiler flags

runtime_CPPFLAGS = -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME
ocamlrun_CPPFLAGS = $(runtime_CPPFLAGS)
ocamlrund_CPPFLAGS = $(runtime_CPPFLAGS) -DDEBUG
ocamlruni_CPPFLAGS = $(runtime_CPPFLAGS) -DCAML_INSTR

## Runtime targets

.PHONY: runtime-all
runtime-all: \
  $(runtime_BYTECODE_STATIC_LIBRARIES) \
  $(runtime_BYTECODE_SHARED_LIBRARIES:%=runtime/lib%_shared$(EXT_DLL)) \
  $(runtime_PROGRAMS:%=runtime/%$(EXE)) $(SAK)

.PHONY: runtime-allopt
ifeq "$(NATIVE_COMPILER)" "true"
runtime-allopt: \
  $(runtime_NATIVE_STATIC_LIBRARIES) \
  $(runtime_NATIVE_SHARED_LIBRARIES:%=runtime/lib%_shared$(EXT_DLL))
else
runtime-allopt:
	$(error The build has been configured with --disable-native-compiler)
endif

## Generated non-object files

runtime/primitives: runtime/gen_primitives.sh $(runtime_BYTECODE_C_SOURCES)
	$(V_GEN)runtime/gen_primitives.sh $@ $(runtime_BYTECODE_C_SOURCES)

runtime/prims.c: runtime/gen_primsc.sh runtime/primitives
	$(V_GEN)runtime/gen_primsc.sh \
                    runtime/primitives $(runtime_BYTECODE_C_SOURCES) \
                    > $@

$(SAK): runtime/sak.c runtime/caml/misc.h runtime/caml/config.h
	$(V_MKEXE)$(call SAK_BUILD,$@,$<)

C_LITERAL = $(shell $(SAK) $(ENCODE_C_LITERAL) $(call QUOTE_SINGLE,$(1)))

runtime/build_config.h: $(ROOTDIR)/Makefile.config \
                        $(ROOTDIR)/Makefile.build_config $(SAK)
	$(V_GEN){ \
	  echo '/* This file is generated from $(ROOTDIR)/Makefile.config */'; \
	  printf '#define OCAML_STDLIB_DIR %s\n' \
	         $(call QUOTE_SINGLE,$(call C_LITERAL,$(TARGET_LIBDIR))); \
	  echo '#define HOST "$(HOST)"'; \
	  echo '#define BYTECODE_RUNTIME_ID "$(BYTECODE_RUNTIME_ID)"'; \
	} > $@

runtime/prims.$(O): runtime/build_config.h

## Runtime libraries and programs

runtime/ocamlrun$(EXE): runtime/prims.$(O) runtime/libcamlrun.$(A)
	$(V_MKEXE)$(MKEXE) -o $@ $^ $(BYTECCLIBS)

runtime/ocamlruns$(EXE): runtime/prims.$(O) runtime/libcamlrun_non_shared.$(A)
	$(V_MKEXE)$(call MKEXE_VIA_CC,$@,$^ $(BYTECCLIBS))

runtime/libcamlrun.$(A): $(libcamlrun_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libcamlrun_non_shared.$(A): $(libcamlrun_non_shared_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/ocamlrund$(EXE): runtime/prims.$(O) runtime/libcamlrund.$(A)
	$(V_MKEXE)$(MKEXE) $(MKEXEDEBUGFLAG) -o $@ $^ $(BYTECCLIBS)

runtime/libcamlrund.$(A): $(libcamlrund_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/ocamlruni$(EXE): runtime/prims.$(O) runtime/libcamlruni.$(A)
	$(V_MKEXE)$(MKEXE) -o $@ $^ $(INSTRUMENTED_RUNTIME_LIBS) $(BYTECCLIBS)

runtime/libcamlruni.$(A): $(libcamlruni_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libcamlrun_pic.$(A): $(libcamlrunpic_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libcamlrun_shared.$(SO): $(libcamlrunpic_OBJECTS)
	$(V_MKDLL)$(MKDLL) -o $@ $^ $(BYTECCLIBS)

runtime/libasmrun.$(A): $(libasmrun_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmrund.$(A): $(libasmrund_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmruni.$(A): $(libasmruni_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmrun_pic.$(A): $(libasmrunpic_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmrun_shared.$(SO): $(libasmrunpic_OBJECTS)
	$(V_MKDLL)$(MKDLL) -o $@ $^ $(NATIVECCLIBS)

runtime/libcomprmarsh.$(A): $(libcomprmarsh_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

## Runtime target-specific preprocessor and compiler flags

runtime/%.b.$(O): OC_CFLAGS = $(OC_BYTECODE_CFLAGS)
runtime/%.b.$(O): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrun_CPPFLAGS)
$(DEPDIR)/runtime/%.b.$(D): \
  OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrun_CPPFLAGS)

runtime/%.bd.$(O): OC_CFLAGS = $(OC_BYTECODE_CFLAGS)
runtime/%.bd.$(O): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrund_CPPFLAGS)
$(DEPDIR)/runtime/%.bd.$(D): \
  OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrund_CPPFLAGS)

runtime/%.bi.$(O): OC_CFLAGS = $(OC_BYTECODE_CFLAGS)
runtime/%.bi.$(O): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlruni_CPPFLAGS)
$(DEPDIR)/runtime/%.bi.$(D): \
  OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlruni_CPPFLAGS)

runtime/%.bpic.$(O): OC_CFLAGS = $(OC_BYTECODE_CFLAGS) $(SHAREDLIB_CFLAGS)
runtime/%.bpic.$(O): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrun_CPPFLAGS)
$(DEPDIR)/runtime/%.bpic.$(D): \
  OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrun_CPPFLAGS)

runtime/%.n.$(O): OC_CFLAGS = $(OC_NATIVE_CFLAGS)
runtime/%.n.$(O): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrun_CPPFLAGS)
$(DEPDIR)/runtime/%.n.$(D): \
  OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrun_CPPFLAGS)

runtime/%.nd.$(O): OC_CFLAGS = $(OC_NATIVE_CFLAGS)
runtime/%.nd.$(O): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrund_CPPFLAGS)
$(DEPDIR)/runtime/%.nd.$(D): \
  OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrund_CPPFLAGS)

runtime/%.ni.$(O): OC_CFLAGS = $(OC_NATIVE_CFLAGS)
runtime/%.ni.$(O): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlruni_CPPFLAGS)
$(DEPDIR)/runtime/%.ni.$(D): \
  OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlruni_CPPFLAGS)

runtime/%.npic.$(O): OC_CFLAGS = $(OC_NATIVE_CFLAGS) $(SHAREDLIB_CFLAGS)
runtime/%.npic.$(O): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrun_CPPFLAGS)
$(DEPDIR)/runtime/%.npic.$(D): \
  OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrun_CPPFLAGS)

## Compilation of C files

# There are two scenarios in which a C object may need to be rebuilt:
# 1. The C source file is newer than the object
# 2. A file #include'd by the C source file is newer than the object
#
# When the C source file is newer than the object, we do not have to care about
# which included files are newer, we just have to be sure that all the files
# which may be #include'd definitely exist.
#
# GCC and clang can both generate the precise dependency information
# needed for (2) as part of compiling the C file. We therefore use C dependency
# information lazily.

# All C files must depend on the _presence_ of the $(runtime_BUILT_HEADERS). If
# a C file actually uses one of these headers, then the dependency will be
# recorded in the .d file (and becomes a real dependency, not an order-only
# dependency).
runtime_MISSING_BUILT_HEADERS = \
  $(filter-out $(wildcard $(runtime_BUILT_HEADERS)), $(runtime_BUILT_HEADERS))

# COMPILE_C_FILE generates the compilation rules for C objects
#   $1 = target pattern for the generated object file (without the .$(O))
#   $2 = source pattern for the C source file (without the .c)
#   $3 = optional; if non-empty suppresses the generation of dependency rules
define DO_COMPILE_C_FILE
ifeq "$(COMPUTE_DEPS)" "true"
# Secondary expansion means we can use @D to depend on the directory being
# created.
$(1).$(O): $(2).c \
  $(if $(3),,| $(DEPDIR)/$$$$(@D) $(runtime_MISSING_BUILT_HEADERS))
else
$(1).$(O): $(2).c \
  $(runtime_CONFIGURED_HEADERS) $(runtime_BUILT_HEADERS) \
  $(RUNTIME_HEADERS)
endif # ifeq "$(COMPUTE_DEPS)" "true"
	$$(V_CC)$$(CC) $$(OC_CFLAGS) $$(CFLAGS) $$(OC_CPPFLAGS) $$(CPPFLAGS) \
	  $$(OUTPUTOBJ)$$@ \
	  $(if $(3),,$(call DEP_FLAGS,$$@,$(DEPDIR)/$$(@:.$(O)=.$(D)))) \
	  -c $$<
# This is skipped if either $(COMPUTE_DEPS) is false or $(3) is non-empty
ifeq "$(COMPUTE_DEPS)$(3)" "true"
# MSVC doesn't emit usable dependency information, but if GCC is available
# then it can instead be called in order to generate the .d file.
ifneq "$(CC)" "$(DEP_CC)"
	$$(V_CCDEPS)$$(DEP_CC) $(DEP_CPPFLAGS) $$(OC_CPPFLAGS) $$(CPPFLAGS) $$< \
   -MM -MT $$@ -MF $(DEPDIR)/$$(@:.$(O)=.$(D))
endif # ifneq "$(CC)" "$(DEP_CC)"
endif # ifeq "$(COMPUTE_DEPS)$(3)" "true"
endef

# The additional call expands the optional $(3) to an empty string
COMPILE_C_FILE = \
  $(call DO_COMPILE_C_FILE,$(1),$(2),$\
                           $(if $(filter-out undefined,$(origin 3)),$(3)))

.PRECIOUS: $(DEPDIR)/%
$(DEPDIR)/%:
	$(MKDIR) $@

runtime_OBJECT_TYPES = % %.b %.bd %.bi %.bpic
ifeq "$(NATIVE_COMPILER)" "true"
runtime_OBJECT_TYPES += %.n %.nd %.ni %.np %.npic
endif

$(foreach runtime_OBJECT_TYPE, $(runtime_OBJECT_TYPES), \
  $(eval $(call COMPILE_C_FILE,runtime/$(runtime_OBJECT_TYPE),runtime/%)))

runtime/$(UNIX_OR_WIN32)_non_shared.%.$(O): \
  OC_CPPFLAGS += -DBUILDING_LIBCAMLRUNS

$(eval $(call COMPILE_C_FILE,runtime/$(UNIX_OR_WIN32)_non_shared.%, \
  runtime/$(UNIX_OR_WIN32)))

$(foreach runtime_OBJECT_TYPE,$(subst %,,$(runtime_OBJECT_TYPES)), \
  $(eval \
    runtime/dynlink$(runtime_OBJECT_TYPE).$(O): $(ROOTDIR)/Makefile.config))

$(eval $(call COMPILE_C_FILE,yacc/%,yacc/%,no-deps))

## Compilation of runtime assembly files

ASPP_ERROR = \
  { echo "If your assembler produced syntax errors, it is probably";\
          echo "unhappy with the preprocessor. Check your assembler, or";\
          echo "try producing $*.o by hand.";\
          exit 2; }
runtime/%.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%.d.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) $(ocamlrund_CPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%.i.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) $(ocamlruni_CPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%_libasmrunpic.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) $(SHAREDLIB_CFLAGS) -o $@ $<

runtime/domain_state.inc: runtime/caml/domain_state.tbl
	$(V_GEN)$(CPP) $< > $@

runtime/amd64nt.obj: runtime/amd64nt.asm runtime/domain_state.inc
	$(V_ASM)$(ASM)$@ $<

runtime/amd64nt.d.obj: runtime/amd64nt.asm runtime/domain_state.inc
	$(V_ASM)$(ASM)$@ $(ocamlrund_CPPFLAGS) $<

runtime/amd64nt.i.obj: runtime/amd64nt.asm runtime/domain_state.inc
	$(V_ASM)$(ASM)$@ $(ocamlruni_CPPFLAGS) $<

runtime/%_libasmrunpic.obj: runtime/%.asm
	$(V_ASM)$(ASM)$@ $<

## Runtime dependencies

RUNTIME_DEP_FILES := $(wildcard $(DEPDIR)/runtime/*.$(D))
.PHONY: $(RUNTIME_DEP_FILES)
include $(RUNTIME_DEP_FILES)

.PHONY: runtime
runtime: stdlib/libcamlrun.$(A)

.PHONY: makeruntime
makeruntime: runtime-all
stdlib/libcamlrun.$(A): runtime-all
	cd stdlib; $(LN) ../runtime/libcamlrun.$(A) .
clean::
	rm -f $(addprefix runtime/, *.o *.obj *.a *.lib *.so *.dll)
	rm -f $(addprefix runtime/, ocamlrun ocamlrund ocamlruni ocamlruns sak)
	rm -f $(addprefix runtime/, \
	  ocamlrun.exe ocamlrund.exe ocamlruni.exe ocamlruns.exe sak.exe)
	rm -f runtime/primitives runtime/primitives*.new runtime/prims.c \
	  $(runtime_BUILT_HEADERS)
	rm -f runtime/domain_state.inc
	rm -rf $(DEPDIR)
	rm -f stdlib/libcamlrun.a stdlib/libcamlrun.lib

.PHONY: runtimeopt
runtimeopt: stdlib/libasmrun.$(A)

.PHONY: makeruntimeopt
makeruntimeopt: runtime-allopt
stdlib/libasmrun.$(A): runtime-allopt
	cd stdlib; $(LN) ../runtime/libasmrun.$(A) .
stdlib/libcomprmarsh.$(A): runtime/libcomprmarsh.$(A)
	cd stdlib; $(LN) ../runtime/libcomprmarsh.$(A) .

clean::
	rm -f stdlib/libasmrun.a stdlib/libasmrun.lib
	rm -f stdlib/libcomprmarsh.a stdlib/libcomprmarsh.lib

# Dependencies

# The following definition duplicates the otherlibs/ prefix but this
# will go away with the merge of the sub makefiles
subdirs = stdlib \
  otherlibs/str \
  otherlibs/systhreads \
  otherlibs/unix \
  otherlibs/runtime_events

.PHONY: alldepend
alldepend: depend
	for dir in $(subdirs); do \
	  $(MAKE) -C $$dir depend || exit; \
	done

# The standard library

.PHONY: library
library: ocamlc
	$(MAKE) -C stdlib all

.PHONY: library-cross
library-cross:
	$(MAKE) -C stdlib OCAMLRUN=../runtime/ocamlrun$(EXE) all

.PHONY: libraryopt
libraryopt:
	$(MAKE) -C stdlib allopt

partialclean::
	$(MAKE) -C stdlib clean

# The lexer generator

ocamllex_LIBRARIES =

ocamllex_SOURCES = $(addprefix lex/,\
  cset.mli cset.ml \
  syntax.mli syntax.ml \
  parser.mly \
  lexer.mli lexer.mll \
  table.mli table.ml \
  lexgen.mli lexgen.ml \
  compact.mli compact.ml \
  common.mli common.ml \
  output.mli output.ml \
  outputbis.mli outputbis.ml \
  main.mli main.ml)

.PHONY: lex-all
lex-all: lex/ocamllex

.PHONY: lex-allopt
lex-allopt: lex/ocamllex.opt

.PHONY: ocamllex
ocamllex: ocamlyacc
	$(MAKE) lex-all

.PHONY: ocamllex.opt
ocamllex.opt: ocamlopt
	$(MAKE) lex-allopt

ocamllex_BYTECODE_LINKFLAGS = -compat-32

ifeq "$(IN_COREBOOT_CYCLE)" "true"
ocamllex_BYTECODE_LINKFLAGS += -set-runtime-default standard_library_default=.
endif

partialclean::
	rm -f lex/*.cm* lex/*.o lex/*.obj \
        $(ocamllex_PROGRAMS) $(ocamllex_PROGRAMS:=.exe) \
        lex/parser.ml lex/parser.mli lex/parser.output \
        lex/lexer.ml

beforedepend:: lex/parser.ml lex/parser.mli lex/lexer.ml

# The ocamlyacc parser generator

ocamlyacc_OTHER_MODULES = $(addprefix yacc/,\
  closure error lalr lr0 main mkpar output reader skeleton symtab \
  verbose warshall)

ocamlyacc_MODULES = $(ocamlyacc_WSTR_MODULE) $(ocamlyacc_OTHER_MODULES)

ocamlyacc_OBJECTS = $(ocamlyacc_MODULES:=.$(O))

# Do not compile assertions in ocamlyacc
ocamlyacc_CPPFLAGS = -DNDEBUG

.PHONY: ocamlyacc
ocamlyacc: $(ocamlyacc_PROGRAM)$(EXE)

$(ocamlyacc_PROGRAM)$(EXE): $(ocamlyacc_OBJECTS)
	$(V_MKEXE)$(MKEXE) -o $@ $^

clean::
	rm -f $(ocamlyacc_MODULES:=.o) $(ocamlyacc_MODULES:=.obj) \
        yacc/wstr.o yacc/wstr.obj

$(ocamlyacc_OTHER_MODULES:=.$(O)): yacc/defs.h

$(ocamlyacc_OTHER_MODULES:=.$(O)): OC_CPPFLAGS += $(ocamlyacc_CPPFLAGS)

# The Menhir-generated parser

# In order to avoid a build-time dependency on Menhir,
# we store the result of the parser generator (which
# are OCaml source files) and Menhir's runtime libraries
# (that the parser files rely on) in boot/.

# The rules below do not depend on Menhir being available,
# they just build the parser from boot/.

# See Makefile.menhir for the rules to rebuild the parser and update
# boot/, which require Menhir. The targets in Makefile.menhir
# (also included here for convenience) must be used after any
# modification of parser.mly.
include Makefile.menhir

# To avoid module-name conflicts with compiler-lib users that link
# with their code with their own MenhirLib module (possibly with
# a different Menhir version), we rename MenhirLib into
# CamlinternalMenhirlib -- and replace the module occurrences in the
# generated parser.ml.

parsing/camlinternalMenhirLib.ml: boot/menhir/menhirLib.ml
	$(V_GEN)cp $< $@
parsing/camlinternalMenhirLib.mli: boot/menhir/menhirLib.mli
	$(V_GEN)echo '[@@@ocaml.warning "-67"]' > $@ && \
	cat $< >> $@

# Copy parsing/parser.ml from boot/

PARSER_DEPS = boot/menhir/parser.ml parsing/parser.mly

ifeq "$(OCAML_DEVELOPMENT_VERSION)" "true"
PARSER_DEPS += tools/check-parser-uptodate-or-warn.sh
endif

parsing/parser.ml: $(PARSER_DEPS)
ifeq "$(OCAML_DEVELOPMENT_VERSION)" "true"
	@-tools/check-parser-uptodate-or-warn.sh
endif
	$(V_GEN)sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@
parsing/parser.mli: boot/menhir/parser.mli
	$(V_GEN)sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@

beforedepend:: parsing/camlinternalMenhirLib.ml \
  parsing/camlinternalMenhirLib.mli \
  parsing/parser.ml parsing/parser.mli

partialclean:: partialclean-menhir


# OCamldoc

# First define the odoc_info library used to build OCamldoc

odoc_info_SOURCES = $(addprefix ocamldoc/,\
  odoc_config.mli odoc_config.ml \
  odoc_messages.mli odoc_messages.ml \
  odoc_global.mli odoc_global.ml \
  odoc_types.mli odoc_types.ml \
  odoc_misc.mli odoc_misc.ml \
  odoc_text_parser.mly \
  odoc_text_lexer.mli odoc_text_lexer.mll \
  odoc_text.mli odoc_text.ml \
  odoc_name.mli odoc_name.ml \
  odoc_parameter.mli odoc_parameter.ml \
  odoc_value.mli odoc_value.ml \
  odoc_type.mli odoc_type.ml \
  odoc_extension.mli odoc_extension.ml \
  odoc_exception.mli odoc_exception.ml \
  odoc_class.mli odoc_class.ml \
  odoc_module.mli odoc_module.ml \
  odoc_print.mli odoc_print.ml \
  odoc_str.mli odoc_str.ml \
  odoc_comments_global.mli odoc_comments_global.ml \
  odoc_parser.mly \
  odoc_lexer.mli odoc_lexer.mll \
  odoc_see_lexer.mli odoc_see_lexer.mll \
  odoc_env.mli odoc_env.ml \
  odoc_merge.mli odoc_merge.ml \
  odoc_sig.mli odoc_sig.ml \
  odoc_ast.mli odoc_ast.ml \
  odoc_search.mli odoc_search.ml \
  odoc_scan.mli odoc_scan.ml \
  odoc_cross.mli odoc_cross.ml \
  odoc_comments.mli odoc_comments.ml \
  odoc_dep.mli odoc_dep.ml \
  odoc_analyse.mli odoc_analyse.ml \
  odoc_info.mli odoc_info.ml)

ocamldoc_LIBRARIES = \
  compilerlibs/ocamlcommon \
  $(addprefix otherlibs/,\
    unix/unix \
    str/str \
    dynlink/dynlink) \
  ocamldoc/odoc_info

ocamldoc_SOURCES = $(addprefix ocamldoc/,\
  odoc_dag2html.mli odoc_dag2html.ml \
  odoc_to_text.mli odoc_to_text.ml \
  odoc_ocamlhtml.mli odoc_ocamlhtml.mll \
  odoc_html.mli odoc_html.ml \
  odoc_man.mli odoc_man.ml \
  odoc_latex_style.mli odoc_latex_style.ml \
  odoc_latex.mli odoc_latex.ml \
  odoc_texi.mli odoc_texi.ml \
  odoc_dot.mli odoc_dot.ml \
  odoc_gen.mli odoc_gen.ml \
  odoc_args.mli odoc_args.ml \
  odoc.mli odoc.ml)

# OCamldoc files to install (a subset of what is built)

OCAMLDOC_LIBMLIS = $(addprefix ocamldoc/,$(addsuffix .mli,\
  odoc_dep odoc_dot odoc_extension odoc_html odoc_info odoc_latex \
  odoc_latex_style odoc_man odoc_messages odoc_ocamlhtml odoc_parameter \
  odoc_texi odoc_text_lexer odoc_to_text odoc_type odoc_value))
OCAMLDOC_LIBCMIS=$(OCAMLDOC_LIBMLIS:.mli=.cmi)
OCAMLDOC_LIBCMTS=$(OCAMLDOC_LIBMLIS:.mli=.cmt) $(OCAMLDOC_LIBMLIS:.mli=.cmti)

ocamldoc/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)
ocamldoc/%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)

ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "false"
# ocamldoc needs a custom runtime when building statically owing to the C stubs
# in unix.cma and str.cma. This is specified explicitly to suppress the default
# linking flags (see $(MAYBE_ADD_BYTECODE_LAUNCHER_FLAGS) in Makefile.common)
ocamldoc/ocamldoc$(EXE): ocamldoc_BYTECODE_LINKFLAGS += -custom
endif

.PHONY: ocamldoc
ocamldoc: ocamldoc/ocamldoc$(EXE) ocamldoc/odoc_test.cmo \
  ocamlc ocamlyacc ocamllex

.PHONY: ocamldoc.opt
ocamldoc.opt: ocamldoc/ocamldoc.opt$(EXE) ocamlopt ocamlyacc ocamllex

# OCamltest

# Libraries ocamltest depends on

ocamltest_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp) \
  $(unix_library)

# List of source files from which ocamltest is compiled
# (all the different sorts of files are derived from this)

# ocamltest has two components: its core and the OCaml "plugin"
# which is actually built into the tool but clearly separated from its core

ocamltest_CORE = \
  run_$(UNIX_OR_WIN32).c run_stubs.c \
  ocamltest_config.ml.in ocamltest_config.mli \
  ocamltest_unix.mli ocamltest_unix.ml \
  ocamltest_stdlib.mli ocamltest_stdlib.ml \
  run_command.mli run_command.ml \
  filecompare.mli filecompare.ml \
  variables.mli variables.ml \
  environments.mli environments.ml \
  result.mli result.ml \
  actions.mli actions.ml \
  tests.mli tests.ml \
  strace.mli strace.ml \
  tsl_ast.mli tsl_ast.ml \
  tsl_parser.mly \
  tsl_lexer.mli tsl_lexer.mll \
  modifier_parser.mli modifier_parser.ml \
  tsl_semantics.mli tsl_semantics.ml \
  builtin_variables.mli builtin_variables.ml \
  actions_helpers.mli actions_helpers.ml \
  builtin_actions.mli builtin_actions.ml \
  translate.mli translate.ml

ocamltest_ocaml_PLUGIN = \
  ocaml_backends.mli ocaml_backends.ml \
  ocaml_filetypes.mli ocaml_filetypes.ml \
  ocaml_variables.mli ocaml_variables.ml \
  ocaml_modifiers.mli ocaml_modifiers.ml \
  ocaml_directories.mli ocaml_directories.ml \
  ocaml_files.mli ocaml_files.ml \
  ocaml_flags.mli ocaml_flags.ml \
  ocaml_commands.mli ocaml_commands.ml \
  ocaml_tools.mli ocaml_tools.ml \
  ocaml_compilers.mli ocaml_compilers.ml \
  ocaml_toplevels.mli ocaml_toplevels.ml \
  ocaml_actions.mli ocaml_actions.ml \
  ocaml_tests.mli ocaml_tests.ml \
  debugger_flags.mli debugger_flags.ml \
  debugger_variables.mli debugger_variables.ml \
  debugger_actions.mli debugger_actions.ml \

ocamltest_SOURCES = $(addprefix ocamltest/, \
  $(ocamltest_CORE) $(ocamltest_ocaml_PLUGIN) \
  options.mli options.ml \
  main.mli main.ml)

$(eval $(call COMPILE_C_FILE,ocamltest/%.b,ocamltest/%))
$(eval $(call COMPILE_C_FILE,ocamltest/%.n,ocamltest/%))

ocamltest_DEPEND_FILES := $(wildcard $(DEPDIR)/ocamltest/*.$(D))
.PHONY: $(ocamltest_DEPEND_FILES)
include $(ocamltest_DEPEND_FILES)

ocamltest/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)

ocamltest/%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)

ocamltest: ocamltest/ocamltest$(EXE) \
  testsuite/lib/lib.cmo testsuite/lib/testing.cma testsuite/tools/expect$(EXE) \
  ocamlc ocamlyacc ocamllex

testsuite/lib/%: VPATH += testsuite/lib

testing_SOURCES = testsuite/lib/testing.mli testsuite/lib/testing.ml
testing_LIBRARIES =

$(addprefix testsuite/lib/testing., cma cmxa): \
  OC_COMMON_LINKFLAGS += -linkall

testsuite/tools/%: VPATH += testsuite/tools

expect_SOURCES = $(addprefix testsuite/tools/,expect.mli expect.ml)
expect_LIBRARIES = $(addprefix compilerlibs/,\
  ocamlcommon ocamlbytecomp ocamltoplevel)

expect_BYTECODE_LINKFLAGS += -linkall

codegen_SOURCES = $(addprefix testsuite/tools/,\
  parsecmmaux.mli parsecmmaux.ml \
  parsecmm.mly \
  lexcmm.mli lexcmm.mll \
  codegen_main.mli codegen_main.ml)
codegen_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamloptcomp)

# The asmgen tests are not ported to MSVC64 yet, so make sure
# to compile the arch specific module they require only if necessary
ifeq "$(CCOMPTYPE)-$(ARCH)" "msvc-amd64"
asmgen_OBJECT =
else
asmgen_MODULE = testsuite/tools/asmgen_$(ARCH)
asmgen_SOURCE = $(asmgen_MODULE).S
asmgen_OBJECT = $(asmgen_MODULE).$(O)
$(asmgen_OBJECT): $(asmgen_SOURCE)
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) -o $@ $< || $(ASPP_ERROR)
endif

test_in_prefix_SOURCES = $(addprefix testsuite/tools/,\
  toolchain.mli toolchain.ml \
  harness.mli harness.ml \
  environment.mli environment.ml \
  cmdline.mli cmdline.ml \
  testBytecodeBinaries.mli testBytecodeBinaries.ml \
  testDynlink.mli testDynlink.ml \
  testLinkModes.mli testLinkModes.ml \
  testRelocation.mli testRelocation.ml \
  testToplevel.mli testToplevel.ml \
  test_ld_conf.mli test_ld_conf.ml \
  test_in_prefix.mli test_in_prefix.ml)
test_in_prefix_LIBRARIES = \
  otherlibs/unix/unix compilerlibs/ocamlcommon compilerlibs/ocamlbytecomp

# test_in_prefix% would only match test_in_prefix.opt, hence the missing 'x'!
testsuite/tools/test_in_prefi%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)

test_in_prefix_BYTECODE_LINKFLAGS += -custom

ifeq "$(TARGET_LIBDIR_IS_RELATIVE)" "true"
# testsuite/tools/test_in_prefix cannot use a relative stdlib because it is run
# from testsuite/tools, not from the installation tree (the alternative would be
# to compile it directly with the installed compiler)
test_in_prefix_NATIVE_LINKFLAGS =
test_in_prefix_COMMON_LINKFLAGS = \
  -set-runtime-default 'standard_library_default=$(LIBDIR)'
endif

testsuite/tools/test_in_prefi%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)

testsuite/tools/poisonedruntime$(EXE): testsuite/tools/poisonedruntime.$(O)
	$(V_MKEXE)$(call MKEXE_VIA_CC,$@,$^)

$(eval $(call COMPILE_C_FILE,\
  testsuite/tools/main_in_c,testsuite/tools/main_in_c,no-deps))
$(eval $(call COMPILE_C_FILE,\
  testsuite/tools/poisonedruntime,testsuite/tools/poisonedruntime,no-deps))

ocamltest_BYTECODE_LINKFLAGS = -custom -g

ocamltest/ocamltest$(EXE): ocamlc ocamlyacc ocamllex

ocamltest/ocamltest.opt$(EXE): ocamlopt ocamlyacc ocamllex

# ocamltest does _not_ want to have access to the Unix interface by default,
# to ensure functions and types are only used via Ocamltest_stdlib.Unix
# (see #9797)
ocamltest/%: \
  VPATH := $(filter-out $(unix_directory), $(VPATH))

# Ocamltest_unix and the linking of the executable itself should include the
# Unix library, if it's being built.
ocamltest/ocamltest_unix.% \
ocamltest/ocamltest$(EXE) ocamltest/ocamltest.opt$(EXE): \
  VPATH += $(unix_directory)

# For flambda mode, it is necessary for Ocamltest_unix to be compiled with
# -opaque to prevent errors compiling the other modules of ocamltest.
ocamltest/ocamltest_unix.%: \
  OC_COMMON_COMPFLAGS += -opaque
ifeq "$(build_ocamltest)" "true"
ocamltest: ocamltest/ocamltest$(EXE) \
  testsuite/lib/lib.cmo testsuite/lib/testing.cma testsuite/tools/expect$(EXE) \
  ocamlc ocamlyacc ocamllex

ocamltest.opt: ocamltest/ocamltest.opt$(EXE) \
  testsuite/lib/testing.cmxa $(asmgen_OBJECT) testsuite/tools/codegen$(EXE) \
  ocamlopt ocamlyacc ocamllex
else # ifeq "$(build_ocamltest)" "true"
ocamltest_TARGETS = ocamltest ocamltest.opt
.PHONY: $(ocamltest_TARGETS)
$(ocamltest_TARGETS):
	@echo ocamltest is disabled
	@echo To build it, run configure again with --enable-ocamltest
	@false
endif # ifeq "$(build_ocamltest)" "true"

partialclean::
	rm -f ocamltest/ocamltest ocamltest/ocamltest.exe
	rm -f ocamltest/ocamltest.opt ocamltest/ocamltest.opt.exe
	rm -f $(addprefix ocamltest/,*.o *.obj *.cm*)
	rm -f $(patsubst %.mll,%.ml, $(wildcard ocamltest/*.mll))
	rm -f $(patsubst %.mly,%.ml, $(wildcard ocamltest/*.mly))
	rm -f $(patsubst %.mly,%.mli, $(wildcard ocamltest/*.mly))
	rm -f $(patsubst %.mly,%.output, $(wildcard ocamltest/*.mly))
	rm -f ocamltest/ocamltest.html
	rm -f $(addprefix testsuite/lib/*.,cm* o obj a lib)
	rm -f $(addprefix testsuite/tools/*.,cm* o obj a lib)
	rm -f testsuite/tools/codegen testsuite/tools/codegen.exe
	rm -f testsuite/tools/poisonedruntime testsuite/tools/poisonedruntime.exe
	rm -f testsuite/tools/expect testsuite/tools/expect.exe
	rm -f testsuite/tools/test_in_prefix testsuite/tools/test_in_prefix.exe
	rm -f testsuite/tools/test_in_prefix.opt \
        testsuite/tools/test_in_prefix.opt.exe
	rm -f testsuite/tools/lexcmm.ml
	rm -f $(addprefix testsuite/tools/parsecmm., ml mli output)

ocamltest/ocamltest_config.ml ocamltest/ocamltest_unix.ml: config.status
	./$< $@

beforedepend:: ocamltest/ocamltest_config.ml ocamltest/ocamltest_unix.ml

# Documentation

.PHONY: html_doc
html_doc: ocamldoc
	$(MAKE) -C api_docgen html

.PHONY: manpages
manpages:
	$(MAKE) -C api_docgen man

partialclean::
	rm -f ocamldoc/ocamldoc ocamldoc/ocamldoc.exe
	rm -f ocamldoc/ocamldoc.opt ocamldoc/ocamldoc.opt.exe
	rm -f ocamldoc/\#*\#
	rm -f ocamldoc/*.cm[aiotx] ocamldoc/*.cmxa ocamldoc/*.cmti \
	  ocamldoc/*.a ocamldoc/*.lib ocamldoc/*.o ocamldoc/*.obj
	rm -f ocamldoc/odoc_parser.output ocamldoc/odoc_text_parser.output
	rm -f ocamldoc/odoc_lexer.ml ocamldoc/odoc_text_lexer.ml \
	  ocamldoc/odoc_see_lexer.ml ocamldoc/odoc_ocamlhtml.ml
	rm -f ocamldoc/odoc_parser.ml ocamldoc/odoc_parser.mli \
	  ocamldoc/odoc_text_parser.ml ocamldoc/odoc_text_parser.mli

partialclean::
	$(MAKE) -C api_docgen clean

# The OCamltest manual

.PHONY: ocamltest-manual
ocamltest-manual: ocamltest/ocamltest.html

ocamltest/ocamltest.html: ocamltest/OCAMLTEST.org
	pandoc -s --toc -N -f org -t html -o $@ $<

# The extra libraries

.PHONY: otherlibraries
otherlibraries: ocamltools dynlink-all
	$(MAKE) -C otherlibs all

.PHONY: otherlibrariesopt
otherlibrariesopt: dynlink-allopt
	$(MAKE) -C otherlibs allopt

otherlibs/unix/unix.cmxa: otherlibrariesopt
otherlibs/str/str.cmxa: otherlibrariesopt

partialclean::
	rm -f otherlibs/dynlink/*.cm[ioaxt] otherlibs/dynlink/*.cmti \
	  otherlibs/dynlink/*.cmxa otherlibs/dynlink/byte/*.cm[iot] \
	  otherlibs/dynlink/byte/*.cmti otherlibs/dynlink/native/*.cm[ixt] \
	  otherlibs/dynlink/native/*.cmti otherlibs/dynlink/native/*.o \
	  otherlibs/dynlink/native/*.obj
	$(MAKE) -C otherlibs partialclean

clean::
	rm -f otherlibs/dynlink/*.a otherlibs/dynlink/*.lib \
	  otherlibs/dynlink/*.o otherlibs/dynlink/*.obj \
	  otherlibs/dynlink/*.so otherlibs/dynlink/*.dll \
	  otherlibs/dynlink/byte/dynlink.mli \
	  otherlibs/dynlink/native/dynlink.mli \

	$(MAKE) -C otherlibs clean

# The replay debugger

ocamldebug_LIBRARIES = compilerlibs/ocamlcommon \
  $(addprefix otherlibs/,unix/unix dynlink/dynlink)

# The following dependencies are necessary at the moment, because the
# root Makefile does not know yet how to build the other libraries
# Once their build will happen in this root Makefile, too, it will become
# possible to get rid of these dependencies

otherlibs/unix/unix.cma: otherlibraries
otherlibs/str/str.cma: otherlibraries

debugger/%: VPATH += otherlibs/unix otherlibs/dynlink

ocamldebug_COMPILER_SOURCES = $(addprefix toplevel/, \
  genprintval.mli genprintval.ml \
  topprinters.mli topprinters.ml)

# The modules listed in the following variable are packed into ocamldebug.cmo

ocamldebug_DEBUGGER_SOURCES = $(addprefix debugger/,\
  int64ops.mli int64ops.ml \
  primitives.mli primitives.ml \
  unix_tools.mli unix_tools.ml \
  debugger_config.mli debugger_config.ml \
  parameters.mli parameters.ml \
  debugger_lexer.mli debugger_lexer.mll \
  input_handling.mli input_handling.ml \
  question.mli question.ml \
  debugcom.mli debugcom.ml \
  exec.mli exec.ml \
  source.mli source.ml \
  pos.mli pos.ml \
  checkpoints.mli checkpoints.ml \
  events.mli events.ml \
  program_loading.mli program_loading.ml \
  symbols.mli symbols.ml \
  breakpoints.mli breakpoints.ml \
  trap_barrier.mli trap_barrier.ml \
  history.mli history.ml \
  printval.mli printval.ml \
  show_source.mli show_source.ml \
  time_travel.mli time_travel.ml \
  program_management.mli program_management.ml \
  frames.mli frames.ml \
  eval.mli eval.ml \
  show_information.mli show_information.ml \
  loadprinter.mli loadprinter.ml \
  debugger_parser.mly \
  command_line.mli command_line.ml \
  main.mli main.ml)

ocamldebug_DEBUGGER_OBJECTS = \
  $(patsubst %.ml, %.cmo, \
    $(patsubst %.mll, %.cmo, \
      $(patsubst %.mly, %.cmo, \
        $(filter-out %.mli, $(ocamldebug_DEBUGGER_SOURCES)))))

ocamldebug_SOURCES = \
  $(ocamldebug_COMPILER_SOURCES) \
  $(addprefix debugger/, \
    ocamldebug.ml \
    ocamldebug_entry.mli ocamldebug_entry.ml)

ocamldebug_BYTECODE_LINKFLAGS = -linkall

debugger/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)

.PHONY: ocamldebug ocamldebugger
ocamldebug: debugger/ocamldebug$(EXE) ocamlc ocamlyacc ocamllex
ocamldebugger: debugger/ocamldebug$(EXE) ocamlc ocamlyacc ocamllex
# the 'ocamldebugger' target is an alias of 'ocamldebug' for
# backward-compatibility with old ./configure scripts; it can be
# removed after most contributors have re-run ./configure once, for
# example after 5.2 is branched

$(ocamldebug_DEBUGGER_OBJECTS): OC_COMMON_COMPFLAGS += -for-pack ocamldebug
debugger/ocamldebug.cmo: $(ocamldebug_DEBUGGER_OBJECTS)
	$(V_OCAMLC)$(CAMLC) $(OC_COMMON_COMPFLAGS) -pack -o $@ $^

debugger/ocamldebug_entry.cmo: debugger/ocamldebug.cmo

ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "false"
# ocamldebug needs a custom runtime when building statically owing to the
# C stubs in unix.cma. This is specified explicitly to suppress the default
# linking flags (see $(MAYBE_ADD_BYTECODE_LAUNCHER_FLAGS) in Makefile.common)
debugger/ocamldebug$(EXE): ocamldebug_BYTECODE_LINKFLAGS += -custom
endif

clean::
	rm -f debugger/ocamldebug debugger/ocamldebug.exe
	rm -f debugger/debugger_lexer.ml
	rm -f $(addprefix debugger/debugger_parser.,ml mli output)

beforedepend:: debugger/debugger_lexer.ml

beforedepend:: debugger/debugger_parser.ml debugger/debugger_parser.mli

# Check that the native-code compiler is supported
.PHONY: checknative
checknative:
ifeq "$(NATIVE_COMPILER)" "false"
	$(error The source tree was configured with --disable-native-compiler!)
else
ifeq "$(ARCH)" "none"
	$(error The native-code compiler is not supported on this platform)
else
	@
endif
endif

# Check that the stack limit is reasonable (Unix-only)
$(eval $(call COMPILE_C_FILE,tools/checkstack,tools/checkstack,no-deps))
.PHONY: checkstack
ifeq "$(UNIX_OR_WIN32)" "unix"
checkstack: tools/checkstack$(EXE)
	$<

.INTERMEDIATE: tools/checkstack$(EXE) tools/checkstack.$(O)
tools/checkstack$(EXE): tools/checkstack.$(O)
	$(V_MKEXE)$(MKEXE) $(OUTPUTEXE)$@ $<
else
checkstack:
	@
endif

# Lint @since and @deprecated annotations

lintapidiff_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp) \
  otherlibs/str/str
lintapidiff_SOURCES = tools/lintapidiff.mli tools/lintapidiff.ml

tools/lintapidiff.opt$(EXE): VPATH += otherlibs/str

VERSIONS=$(shell git tag|grep '^[0-9]*.[0-9]*.[0-9]*$$'|grep -v '^[12].')
.PHONY: lintapidiff
lintapidiff: tools/lintapidiff.opt$(EXE)
	git ls-files -- 'otherlibs/*/*.mli' 'stdlib/*.mli' |\
	    grep -Ev internal\|obj\|stdLabels\|moreLabels |\
	    tools/lintapidiff.opt $(VERSIONS)

# Regenerate otherlibs/dynlink/byte/dynlink_symtable from its bytecomp sources

sync_dynlink_SOURCES = tools/sync_dynlink.mli tools/sync_dynlink.ml
sync_dynlink_LIBRARIES =

.PHONY: sync_dynlink
sync_dynlink: tools/sync_dynlink.opt$(EXE)
	    ./tools/sync_dynlink.opt$(EXE) \
        otherlibs/dynlink/byte/dynlink_symtable.ml \
      > synced_dynlink.tmp
	    diff -u synced_dynlink.tmp otherlibs/dynlink/byte/dynlink_symtable.ml
	    rm synced_dynlink.tmp
# Tools

TOOLS_BYTECODE_TARGETS = \
  $(TOOLS_NAT_PROGRAMS) $(TOOLS_BYT_PROGRAMS) $(TOOLS_MODULES:=.cmo)

TOOLS_NATIVE_TARGETS = $(TOOLS_MODULES:=.cmx)

TOOLS_OPT_TARGETS = $(TOOLS_NAT_PROGRAMS:=.opt)

.PHONY: ocamltools
ocamltools: ocamlc ocamllex
	$(MAKE) tools-all

.PHONY: tools-all
tools-all: $(TOOLS_BYTECODE_TARGETS)

.PHONY: tools-allopt
tools-allopt: $(TOOLS_NATIVE_TARGETS)

.PHONY: tools-allopt.opt
tools-allopt.opt: $(TOOLS_OPT_TARGETS)

.PHONY: ocamltoolsopt
ocamltoolsopt: ocamlopt
	$(MAKE) tools-allopt

.PHONY: ocamltoolsopt.opt
ocamltoolsopt.opt: ocamlc.opt ocamllex.opt
	$(MAKE) tools-allopt.opt

# Tools that require a full ocaml distribution: otherlibs and toplevel

OTHER_TOOLS =

ocamltex = tools/ocamltex$(EXE)

ifeq "$(build_ocamltex)" "true"
OTHER_TOOLS += $(ocamltex)
endif

.PHONY: othertools
othertools: $(OTHER_TOOLS)

partialclean::
	for prefix in cm* dll so lib a obj; do \
	  rm -f tools/*.$$prefix; \
	done

# The dependency generator

ocamldep_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
ocamldep_SOURCES = tools/ocamldep.mli tools/ocamldep.ml

ocamldep_BYTECODE_LINKFLAGS = -compat-32

# The profiler

ocamlprof_LIBRARIES =
ocamlprof_SOURCES = \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  format_doc.mli format_doc.ml \
  misc.mli misc.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  terminfo.mli terminfo.ml \
  warnings.mli warnings.ml \
  location.mli location.ml \
  longident.mli longident.ml \
  docstrings.mli docstrings.ml \
  syntaxerr.mli syntaxerr.ml \
  ast_helper.mli ast_helper.ml \
  ast_iterator.mli ast_iterator.ml \
  builtin_attributes.mli builtin_attributes.ml \
  camlinternalMenhirLib.mli camlinternalMenhirLib.ml \
  parser.mli parser.ml \
  lexer.mli lexer.ml \
  pprintast.mli pprintast.ml \
  parse.mli parse.ml \
  ocamlprof.mli ocamlprof.ml

ocamlcp_ocamloptp_SOURCES = \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  format_doc.mli format_doc.ml \
  misc.mli misc.ml \
  profile.mli profile.ml \
  warnings.mli warnings.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  terminfo.mli terminfo.ml \
  location.mli location.ml \
  ccomp.mli ccomp.ml \
  compenv.mli compenv.ml \
  main_args.mli main_args.ml \
  ocamlcp_common.mli ocamlcp_common.ml

ocamlcp_LIBRARIES =
ocamlcp_SOURCES = $(ocamlcp_ocamloptp_SOURCES) ocamlcp.mli ocamlcp.ml

ocamloptp_LIBRARIES =
ocamloptp_SOURCES = $(ocamlcp_ocamloptp_SOURCES) ocamloptp.mli ocamloptp.ml

# To help building mixed-mode libraries (OCaml + C)
ocamlmklib_LIBRARIES =
ocamlmklib_SOURCES = \
  config.ml \
  build_path_prefix_map.ml \
  format_doc.ml \
  misc.ml \
  ocamlmklib.mli ocamlmklib.ml

# To make custom toplevels

ocamlmktop_LIBRARIES =
ocamlmktop_SOURCES = \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  format_doc.mli format_doc.ml \
  misc.mli misc.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  profile.mli profile.ml \
  ccomp.mli ccomp.ml \
  ocamlmktop.mli ocamlmktop.ml

# Reading cmt files

ocamlcmt_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
ocamlcmt_SOURCES = tools/ocamlcmt.mli tools/ocamlcmt.ml

# The bytecode disassembler

dumpobj_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
dumpobj_SOURCES = $(addprefix tools/, \
  opnames.mli opnames.ml \
  dumpobj.mli dumpobj.ml)

tools/opnames.ml: tools/opnames.ml.c runtime/caml/opcodes.h
	$(V_GEN)$(CPP) -I runtime $< > $@

clean::
	rm -f tools/opnames.ml

beforedepend:: tools/opnames.ml

# Display info on compiled files

ocamlobjinfo_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp ocamlmiddleend)
ocamlobjinfo_SOURCES = tools/objinfo.mli tools/objinfo.ml

# Scan object files for required primitives

primreq_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
primreq_SOURCES = tools/primreq.mli tools/primreq.ml

# Copy a bytecode executable, stripping debug info

stripdebug_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
stripdebug_SOURCES = tools/stripdebug.mli tools/stripdebug.ml

# Compare two bytecode executables

cmpbyt_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
cmpbyt_SOURCES = tools/cmpbyt.mli tools/cmpbyt.ml

# Scan latex files, and run ocaml code examples

ocamltex_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp ocamltoplevel) \
  $(addprefix otherlibs/,str/str unix/unix)
ocamltex_SOURCES = tools/ocamltex.mli tools/ocamltex.ml

# ocamltex uses str.cma and unix.cma and so must be compiled with
# $(ROOTDIR)/ocamlc rather than with $(ROOTDIR)/boot/ocamlc since the boot
# compiler does not necessarily have the correct shared library
# configuration.
# Note: the following definitions apply to all the prerequisites
# of ocamltex.
$(ocamltex): CAMLC = $(OCAMLRUN) $(ROOTDIR)/ocamlc$(EXE) $(STDLIBFLAGS)
$(ocamltex): OC_COMMON_LINKFLAGS += -linkall
$(ocamltex): VPATH += $(addprefix otherlibs/,str unix)

tools/ocamltex.cmo: OC_COMMON_COMPFLAGS += -no-alias-deps

ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "false"
# ocamltex needs a custom runtime when building statically owing to the C stubs
# in unix.cma and str.cma. This is specified explicitly to suppress the default
# linking flags (see $(MAYBE_ADD_BYTECODE_LAUNCHER_FLAGS) in Makefile.common)
tools/ocamltex$(EXE): ocamltex_BYTECODE_LINKFLAGS += -custom
endif

# we need str and unix which depend on the bytecode version of other tools
# thus we use the othertools target
## Test compilation of backend-specific parts

ARCH_SPECIFIC =\
  asmcomp/arch.mli asmcomp/arch.ml asmcomp/proc.ml asmcomp/CSE.ml \
  asmcomp/selection.ml asmcomp/scheduling.ml asmcomp/reload.ml \
  asmcomp/stackframe.ml

partialclean::
	rm -f $(ARCH_SPECIFIC)

beforedepend:: $(ARCH_SPECIFIC)

# This rule provides a quick way to check that machine-dependent
# files compiles fine for a foreign architecture (passed as ARCH=xxx).

.PHONY: check_arch
check_arch:
	@echo "========= CHECKING asmcomp/$(ARCH) =============="
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*
	@$(MAKE) compilerlibs/ocamloptcomp.cma \
	            >/dev/null
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*

.PHONY: check_all_arches
check_all_arches:
ifeq ($(ARCH64),true)
	@STATUS=0; \
	 for i in $(ARCHES); do \
	   $(MAKE) --no-print-directory check_arch ARCH=$$i || STATUS=1; \
	 done; \
	 exit $$STATUS
else
	 @echo "Architecture tests are disabled on 32-bit platforms."
endif

# The native toplevel

ocamlnat_LIBRARIES = \
  compilerlibs/ocamlcommon compilerlibs/ocamloptcomp \
  compilerlibs/ocamlbytecomp otherlibs/dynlink/dynlink \
  compilerlibs/ocamltoplevel

ocamlnat_SOURCES = $(ocaml_SOURCES)

ocamlnat_NATIVE_LINKFLAGS = -linkall -I toplevel/native

COMPILE_NATIVE_MODULE = \
  $(CAMLOPT) $(OC_COMMON_COMPFLAGS) -I $(@D) $(INCLUDES) \
  $(OC_NATIVE_COMPFLAGS)


toplevel/topdirs.cmx toplevel/toploop.cmx $(ocamlnat_CMX_FILES): \
  OC_NATIVE_COMPFLAGS += -I toplevel/native

toplevel/toploop.cmx: toplevel/native/topeval.cmx

$(ocamlnat_CMX_FILES): toplevel/native/topmain.cmx

partialclean::
	rm -f ocamlnat ocamlnat.exe

toplevel/native/topeval.cmx: otherlibs/dynlink/dynlink.cmxa

# The numeric opcodes

bytecomp/opcodes.ml: bytecomp/opcodes.ml.c runtime/caml/opcodes.h
	$(CPP) -I runtime $< > $@

bytecomp/opcodes.mli: bytecomp/opcodes.ml
	$(V_GEN)$(CAMLC) -i $< > $@

partialclean::
	rm -f bytecomp/opcodes.ml bytecomp/opcodes.mli

beforedepend:: bytecomp/opcodes.ml bytecomp/opcodes.mli

ifneq "$(wildcard .git)" ""
include Makefile.dev
endif

# Default rules

%.cmo: %.ml
	$(V_OCAMLC)$(CAMLC) $(OC_COMMON_COMPFLAGS) -I $(@D) $(INCLUDES) -c $<

%.cmi: %.mli
	$(V_OCAMLC)$(CAMLC) $(OC_COMMON_COMPFLAGS) -I $(@D) $(INCLUDES) -c $<

%.cmx: %.ml
	$(V_OCAMLOPT)$(COMPILE_NATIVE_MODULE) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp middle_end file_formats \
           lambda middle_end/closure middle_end/flambda \
           middle_end/flambda/base_types \
           driver toplevel toplevel/byte toplevel/native tools debugger; do \
	  rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.s $$d/*.asm \
	    $$d/*.o $$d/*.obj $$d/*.so $$d/*.dll; \
	done

%.depend: beforedepend
	$(V_OCAMLDEP)$(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I $* $(INCLUDES) \
	  $(OCAMLDEPFLAGS) $*/*.mli $*/*.ml > $@

asmcomp.depend:: beforedepend $(cvt_emit)
	$(V_OCAMLDEP)$(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I asmcomp $(INCLUDES) \
	  $(OCAMLDEPFLAGS) $(filter-out $(ARCH_SPECIFIC) asmcomp/emit.ml, \
	                                $(wildcard asmcomp/*.mli asmcomp/*.ml)) > $@

partialclean::
	rm -f $(addsuffix .depend, $(ARCH_SPECIFIC) asmcomp/emit.ml)

# asmcomp.depend contains the dependencies for all the backends, with ifeq used
# to select the correct one depending on the ARCH variable. In order to
# generate this file, we must temporarily replace the $(ARCH_SPECIFIC) files
# with the ones for each architecture. At the end of this process, the files for
# the active architecture (i.e. $(ARCH)) must be restored, but if we simply
# re-link the files we will trigger a complete rebuild of the native compiler
# and .opt binaries. The recipe for asmcomp.depend therefore begins by renaming
# the existing files, then it generates asmcomp.depend and then we rename the
# files back. This means their timestamps are unaltered, and the next invocation
# of make therefore correctly doesn't rebuild anything.

define MV_FILE
asmcomp.depend::
	@mv $(1) $(2)

endef

$(foreach file, asmcomp/emit.ml $(ARCH_SPECIFIC),\
  $(eval $(call MV_FILE,$(file),$(file).depend)))

define ADD_ARCH_SPECIFIC_DEPS
asmcomp.depend::
	@echo 'ifeq "$$$$(ARCH)" "$(1)"' > asmcomp/$(1).depend
	@$$(MAKE) ARCH=$(1) asmcomp/emit.ml $$(ARCH_SPECIFIC)
	@$$(OCAMLDEP) $$(OC_OCAMLDEPFLAGS) -I asmcomp $$(INCLUDES) \
	  $$(OCAMLDEPFLAGS) asmcomp/emit.ml $$(ARCH_SPECIFIC) >> asmcomp/$(1).depend
	@echo 'endif # ifeq "$$$$(ARCH)" "$(1)"' >> asmcomp/$(1).depend
	@rm -f asmcomp/emit.ml $$(ARCH_SPECIFIC)

endef

$(foreach arch, $(ARCHES),\
  $(eval $(call ADD_ARCH_SPECIFIC_DEPS,$(arch))))

asmcomp.depend::
	@cat $(addprefix asmcomp/, $(addsuffix .depend, $(ARCHES))) >> $@
	@rm -f $(addprefix asmcomp/, $(addsuffix .depend, $(ARCHES)))

$(foreach file, asmcomp/emit.ml $(ARCH_SPECIFIC),\
  $(eval $(call MV_FILE,$(file).depend,$(file))))

DEP_DIRS = \
  utils parsing typing bytecomp asmcomp middle_end lambda file_formats \
  middle_end/closure middle_end/flambda middle_end/flambda/base_types driver \
  toplevel toplevel/byte toplevel/native lex tools debugger ocamldoc ocamltest \
  testsuite/lib testsuite/tools otherlibs/dynlink

DEP_FILES = $(addsuffix .depend, $(DEP_DIRS))

.INTERMEDIATE: $(DEP_FILES)

.PHONY: depend
depend: $(DEP_FILES) | beforedepend
	$(V_GEN)cat $^ > .$@
	@rm -f $(DYNLINK_DEPEND_DUMMY_FILES)
=======
clean::
	$(MAKE) -C testsuite clean

# Build the manual latex files from the etex source files
# (see manual/README.md)
.PHONY: manual-pregen
manual-pregen: opt.opt
	cd manual; $(MAKE) clean && $(MAKE) pregen-etex

clean::
	$(MAKE) -C manual clean

# The clean target
clean:: partialclean
	rm -f configure~
	rm -f $(C_PROGRAMS) $(C_PROGRAMS:=.exe)
	rm -f $(OCAML_PROGRAMS) $(OCAML_PROGRAMS:=.exe)
	rm -f $(OCAML_PROGRAMS:=.opt) $(OCAML_PROGRAMS:=.opt.exe)
	rm -f $(OCAML_BYTECODE_PROGRAMS) $(OCAML_BYTECODE_PROGRAMS:=.exe)
	rm -f $(OCAML_NATIVE_PROGRAMS) $(OCAML_NATIVE_PROGRAMS:=.exe)

# The bytecode compiler

ocamlc_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)

ocamlc_SOURCES = driver/main.mli driver/main.ml

ocamlc_BYTECODE_LINKFLAGS = -compat-32 -g

ifeq "$(IN_COREBOOT_CYCLE)" "true"
ocamlc_BYTECODE_LINKFLAGS += -set-runtime-default standard_library_default=.
endif

partialclean::
	rm -f ocamlc ocamlc.exe ocamlc.opt ocamlc.opt.exe ocamlc*.stripped

# The native-code compiler

ocamlopt_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamloptcomp)

ocamlopt_SOURCES = driver/optmain.mli driver/optmain.ml

ocamlopt_BYTECODE_LINKFLAGS = -g

partialclean::
	rm -f ocamlopt ocamlopt.exe ocamlopt.opt ocamlopt.opt.exe ocamlopt*.stripped

# The toplevel

# At the moment, the toplevel can't be built with the general build macros
# because its build involves calling expunge. We thus give its build
# rules explicitly until the day expunge can hopefully be removed.

ocaml_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp ocamltoplevel)

ocaml_CMA_FILES = $(ocaml_LIBRARIES:=.cma)

ocaml_SOURCES = toplevel/topstart.mli toplevel/topstart.ml

ocaml_CMO_FILES = toplevel/topstart.cmo

.INTERMEDIATE: ocaml.tmp
ocaml.tmp: OC_BYTECODE_LINKFLAGS += -I toplevel/byte -linkall -g
ocaml.tmp: $(ocaml_CMA_FILES) $(ocaml_CMO_FILES)
	$(V_LINKC)$(LINK_BYTECODE_PROGRAM) -o $@ $^

$(eval $(call PROGRAM_SYNONYM,ocaml))
ocaml$(EXE): $(expunge) ocaml.tmp
	- $(V_GEN)$(OCAMLRUN) $^ $@ $(PERVASIVES)

partialclean::
	rm -f ocaml ocaml.exe

# Use TOPFLAGS to pass additional flags to the bytecode or native toplevel
# when running make runtop or make natruntop
TOPFLAGS ?=
OC_TOPFLAGS = $(STDLIBFLAGS) -I toplevel -noinit $(TOPINCLUDES) $(TOPFLAGS)

# Use runtime/ocamlrun rather than boot/ocamlrun since boot/ocamlrun is compiled
# without shared library support on Windows (when bootstrapping flexdll)
RUN_OCAML = $(RLWRAP) $(NEW_OCAMLRUN) ./ocaml$(EXE) $(OC_TOPFLAGS)
RUN_OCAMLNAT = $(RLWRAP) ./ocamlnat$(EXE) $(OC_TOPFLAGS)

# Note: Beware that, since these rules begin with a coldstart, boot/ocamlc must
# produce code capable of being executed using runtime/ocamlrun (i.e. there are
# circumstances where it may be necessary to bootstrap first, but if you're
# doing work which needs it, you probably know that already).
.PHONY: runtop
runtop: coldstart
	$(MAKE) ocamlc
	$(MAKE) ocaml
	@$(RUN_OCAML)

.PHONY: runtop-with-otherlibs
runtop-with-otherlibs: coldstart
	$(MAKE) ocamlc
	$(MAKE) otherlibraries
	$(MAKE) ocaml
	@$(RUN_OCAML)

.PHONY: natruntop
natruntop:
	$(MAKE) core
	$(MAKE) opt
	$(MAKE) ocamlnat
	@$(RUN_OCAMLNAT)

# The dynlink library

dynlink_SOURCES = $(addprefix otherlibs/dynlink/,\
  dynlink_config.mli dynlink_config.ml \
  dynlink_types.mli dynlink_types.ml \
  dynlink_platform_intf.mli dynlink_platform_intf.ml \
  dynlink_common.mli dynlink_common.ml \
  byte/dynlink_symtable.mli byte/dynlink_symtable.ml \
  byte/dynlink.mli byte/dynlink.ml \
  native/dynlink.mli native/dynlink.ml)

dynlink_LIBRARIES =

otherlibs/dynlink/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)
otherlibs/dynlink/%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)


otherlibs/dynlink/%/dynlink.cmi: \
  otherlibs/dynlink/dynlink.cmi otherlibs/dynlink/dynlink.mli
	cp $^ otherlibs/dynlink/$*/

.PHONY: dynlink-all
dynlink-all: otherlibs/dynlink/dynlink.cma

.PHONY: dynlink-allopt
dynlink-allopt: otherlibs/dynlink/dynlink.cmxa

otherlibs/dynlink/dynlink.cma: VPATH += otherlibs/dynlink/byte
otherlibs/dynlink/dynlink.cmxa: VPATH += otherlibs/dynlink/native

ifeq "$(FLAMBDA)" "true"
otherlibs/dynlink/%: OC_NATIVE_COMPFLAGS += -O3
endif

# dynlink.cmx needs to be available in the search path (since
# it is not compiled with -opaque), and we prefer to make the file
# available in a directory that is already searched rather than have
# to add otherlibs/dynlink/native to the search path as well

otherlibs/dynlink/dynlink.cmx : otherlibs/dynlink/native/dynlink.cmx
	cd otherlibs/dynlink; $(LN) native/dynlink.cmx .

DYNLINK_DEPEND_DUMMY_FILES = \
  otherlibs/dynlink/dynlink.ml \
  otherlibs/dynlink/byte/dynlink.mli \
  otherlibs/dynlink/native/dynlink.mli

beforedepend::
	@touch $(DYNLINK_DEPEND_DUMMY_FILES)

otherlibs/dynlink.depend: beforedepend
	@$(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I otherlibs/dynlink $(INCLUDES) \
	  $(OCAMLDEPFLAGS) \
	  -I otherlibs/dynlink/byte \
	  -bytecode otherlibs/dynlink/*.mli otherlibs/dynlink/dynlink_*.ml \
	  otherlibs/dynlink/byte/*.mli otherlibs/dynlink/byte/*.ml \
	  > $@
	@$(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I otherlibs/dynlink $(INCLUDES) \
	  $(OCAMLDEPFLAGS) \
	  -I otherlibs/dynlink/native \
	  -native otherlibs/dynlink/dynlink_*.ml \
	  otherlibs/dynlink/native/dynlink.ml \
	  >> $@

# Cleanup the lexers

partialclean::
	rm -f bytecomp/byterntm.ml parsing/lexer.ml

beforedepend:: bytecomp/byterntm.ml parsing/lexer.ml

# The predefined exceptions and primitives

lambda/runtimedef.ml: lambda/generate_runtimedef.sh runtime/caml/fail.h \
    runtime/primitives
	$(V_GEN)$^ > $@

partialclean::
	rm -f lambda/runtimedef.ml

beforedepend:: lambda/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.mli: asmcomp/$(ARCH)/arch.mli
	@cd asmcomp; $(LN) $(ARCH)/arch.mli .

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	@cd asmcomp; $(LN) $(ARCH)/arch.ml .

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	@cd asmcomp; $(LN) $(ARCH)/proc.ml .

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	@cd asmcomp; $(LN) $(ARCH)/selection.ml .

asmcomp/CSE.ml: asmcomp/$(ARCH)/CSE.ml
	@cd asmcomp; $(LN) $(ARCH)/CSE.ml .

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	@cd asmcomp; $(LN) $(ARCH)/reload.ml .

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	@cd asmcomp; $(LN) $(ARCH)/scheduling.ml .

asmcomp/stackframe.ml: asmcomp/$(ARCH)/stackframe.ml
	@cd asmcomp; $(LN) $(ARCH)/stackframe.ml .

# Preprocess the code emitters
cvt_emit = tools/cvt_emit$(EXE)

beforedepend:: tools/cvt_emit.ml

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp $(cvt_emit)
	$(V_GEN)echo \# 1 \"asmcomp/$(ARCH)/emit.mlp\" > $@ && \
	$(OCAMLRUN) $(cvt_emit) < $< >> $@ \
	|| { rm -f $@; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml tools/cvt_emit.ml

beforedepend:: asmcomp/emit.ml

cvt_emit_LIBRARIES =
cvt_emit_SOURCES = tools/cvt_emit.mli tools/cvt_emit.mll

# The "expunge" utility

expunge_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)

expunge_SOURCES = toplevel/expunge.mli toplevel/expunge.ml

partialclean::
	rm -f expunge expunge.exe

# The runtime system

## Lists of source files

runtime_COMMON_C_SOURCES = \
  addrmap \
  afl \
  alloc \
  array \
  backtrace \
  bigarray \
  blake2 \
  callback \
  codefrag \
  compare \
  custom \
  debugger \
  domain \
  dynlink \
  extern \
  fail \
  fiber \
  finalise \
  floats \
  gc_ctrl \
  gc_stats \
  globroots \
  hash \
  intern \
  ints \
  io \
  lexing \
  lf_skiplist \
  main \
  major_gc \
  md5 \
  memory \
  memprof \
  meta \
  minor_gc \
  misc \
  obj \
  parsing \
  platform \
  printexc \
  prng \
  roots \
  runtime_events \
  shared_heap \
  signals \
  skiplist \
  startup_aux \
  str \
  sync \
  sys \
  $(UNIX_OR_WIN32) \
  weak

runtime_BYTECODE_ONLY_C_SOURCES = \
  backtrace_byt \
  fail_byt \
  fix_code \
  interp \
  startup_byt \
  zstd
runtime_BYTECODE_C_SOURCES = \
  $(runtime_COMMON_C_SOURCES:%=runtime/%.c) \
  $(runtime_BYTECODE_ONLY_C_SOURCES:%=runtime/%.c)

runtime_NATIVE_ONLY_C_SOURCES = \
  $(TSAN_NATIVE_RUNTIME_C_SOURCES) \
  backtrace_nat \
  clambda_checks \
  dynlink_nat \
  fail_nat \
  frame_descriptors \
  startup_nat \
  signals_nat
runtime_NATIVE_C_SOURCES = \
  $(runtime_COMMON_C_SOURCES:%=runtime/%.c) \
  $(runtime_NATIVE_ONLY_C_SOURCES:%=runtime/%.c)

## Header files generated by configure
runtime_CONFIGURED_HEADERS = \
  $(addprefix runtime/caml/, exec.h m.h s.h version.h)

## Header files generated by make
runtime_BUILT_HEADERS = runtime/build_config.h

## Targets to build and install

runtime_PROGRAMS = ocamlrun
runtime_BYTECODE_STATIC_LIBRARIES = runtime/libcamlrun.$(A)
runtime_BYTECODE_SHARED_LIBRARIES =
runtime_NATIVE_STATIC_LIBRARIES = \
  runtime/libasmrun.$(A) runtime/libcomprmarsh.$(A)
runtime_NATIVE_SHARED_LIBRARIES =

ifeq "$(RUNTIMED)" "true"
runtime_PROGRAMS += ocamlrund
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlrund.$(A)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmrund.$(A)
endif

ifeq "$(INSTRUMENTED_RUNTIME)" "true"
runtime_PROGRAMS += ocamlruni
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlruni.$(A)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmruni.$(A)
endif

ifeq "$(UNIX_OR_WIN32)" "unix"
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlrun_pic.$(A)
runtime_BYTECODE_SHARED_LIBRARIES += camlrun
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmrun_pic.$(A)
runtime_NATIVE_SHARED_LIBRARIES += asmrun
endif
endif

## List of object files for each target


libcamlrun_OBJECTS = \
  $(runtime_BYTECODE_C_SOURCES:.c=.b.$(O))

libcamlrun_non_shared_OBJECTS = \
  $(subst $(UNIX_OR_WIN32).b.$(O),$(UNIX_OR_WIN32)_non_shared.b.$(O), \
          $(libcamlrun_OBJECTS))

libcamlrund_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.bd.$(O)) \
  runtime/instrtrace.bd.$(O)

libcamlruni_OBJECTS = \
  $(runtime_BYTECODE_C_SOURCES:.c=.bi.$(O))

libcamlrunpic_OBJECTS = \
  $(runtime_BYTECODE_C_SOURCES:.c=.bpic.$(O))

libasmrun_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.n.$(O)) $(runtime_ASM_OBJECTS)

libasmrund_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.nd.$(O)) $(runtime_ASM_OBJECTS:.$(O)=.d.$(O))

libasmruni_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.ni.$(O)) $(runtime_ASM_OBJECTS:.$(O)=.i.$(O))

libasmrunpic_OBJECTS = $(runtime_NATIVE_C_SOURCES:.c=.npic.$(O)) \
  $(runtime_ASM_OBJECTS:.$(O)=_libasmrunpic.$(O))

libcomprmarsh_OBJECTS = runtime/zstd.npic.$(O)

## General (non target-specific) assembler and compiler flags

runtime_CPPFLAGS = -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME
ocamlrun_CPPFLAGS = $(runtime_CPPFLAGS)
ocamlrund_CPPFLAGS = $(runtime_CPPFLAGS) -DDEBUG
ocamlruni_CPPFLAGS = $(runtime_CPPFLAGS) -DCAML_INSTR

## Runtime targets

.PHONY: runtime-all
runtime-all: \
  $(runtime_BYTECODE_STATIC_LIBRARIES) \
  $(runtime_BYTECODE_SHARED_LIBRARIES:%=runtime/lib%_shared$(EXT_DLL)) \
  $(runtime_PROGRAMS:%=runtime/%$(EXE)) $(SAK)

# If the compiler is configured with --enable-warn-error (which is the default
# for development builds), then all the installed header files are tested with
# more warnings enabled. The test also ensures that each header can be included
# without any other headers.
ifneq "$(EXTRA_WARNINGS_CFLAGS)" ""
runtime-all: runtime-header-tests
endif

# As with the runtime-header-tests, if a C++ compiler is available, then we also
# verify that the headers can be included in a C++ program without error.
ifneq "$(EXTRA_WARNINGS_CXXFLAGS)" ""
ifneq "$(TEST_CXX)" ""
runtime-all: runtime-header-cxx-tests
endif
endif

# Unlike in COMPILE_C_FILE, this is just a convenience test, so we don't do any
# effort with the dependency computations worrying about DEP_CC and so forth,
# because most _development_ is done with feature-rich C compilers!
API_TESTING_DEP_FILES := $(wildcard runtime/api-testing/*.$(D))
.PHONY: $(API_TESTING_DEP_FILES)
include $(API_TESTING_DEP_FILES)

ALL_PUBLIC_HEADER_FILES := $(notdir $(sort $(wildcard \
  $(addsuffix /caml/*.h, $(addprefix otherlibs/, $(OTHERLIBS))) \
  runtime/caml/*.h \
  $(filter runtime/caml/%, $(runtime_BUILT_HEADERS)))))

runtime-header-tests: \
  $(addprefix runtime/api-testing/,$(ALL_PUBLIC_HEADER_FILES:.h=.t))

runtime-header-cxx-tests: \
  $(addprefix runtime/api-testing/,$(ALL_PUBLIC_HEADER_FILES:.h=.tpp))

.PRECIOUS: runtime/api-testing
runtime/api-testing:
	$(MKDIR) $@

runtime/api-testing/%.t: runtime/api-testing/%.c.$(O)
	@touch $@

runtime/api-testing/%.tpp: runtime/api-testing/%.cpp.$(O)
	@touch $@

runtime/api-testing/%.c.$(O): runtime/api-testing/%.c
	$(V_CC)$(CC) \
	  $(OC_CFLAGS) $(EXTRA_WARNINGS_CFLAGS) $(CFLAGS) \
	  $(OC_CPPFLAGS) \
	  $(call DEP_FLAGS,$@,$(@:.$(O)=.$(D))) \
	  $(addprefix -I otherlibs/, $(OTHERLIBS)) $(CPPFLAGS) \
	  $(OUTPUTOBJ)$@ -c $<

runtime/api-testing/%.c: | runtime/api-testing
	$(V_GEN){ \
	  echo '#include <caml/$*.h>'; \
	  echo '#include <caml/$*.h>'; \
	  echo 'int answer = 42;'; \
	} > $@

runtime/api-testing/%.cpp.$(O): runtime/api-testing/%.cpp
	$(V_CXX)$(TEST_CXX) \
	  $(TEST_CXX_CXXFLAGS) $(EXTRA_WARNINGS_CXXFLAGS) $(CXXFLAGS) \
	  $(TEST_CXX_CPPFLAGS) \
	  $(call DEP_FLAGS,$@,$(@:.$(O)=.$(D))) \
	  -I runtime $(addprefix -I otherlibs/, $(OTHERLIBS)) $(CPPFLAGS) \
	  $(OUTPUTOBJ)$@ -c $<

runtime/api-testing/%.cpp: | runtime/api-testing
	$(V_GEN){ \
	  echo '#include <caml/$*.h>'; \
	  echo '#include <caml/$*.h>'; \
	  echo 'int answer = 42;'; \
	} > $@

.PHONY: runtime-allopt
ifeq "$(NATIVE_COMPILER)" "true"
runtime-allopt: \
  $(runtime_NATIVE_STATIC_LIBRARIES) \
  $(runtime_NATIVE_SHARED_LIBRARIES:%=runtime/lib%_shared$(EXT_DLL))
else
runtime-allopt:
	$(error The build has been configured with --disable-native-compiler)
endif

## Generated non-object files

runtime/primitives: runtime/gen_primitives.sh $(runtime_BYTECODE_C_SOURCES)
	$(V_GEN)runtime/gen_primitives.sh $@ $(runtime_BYTECODE_C_SOURCES)

runtime/prims.c: runtime/gen_primsc.sh runtime/primitives
	$(V_GEN)runtime/gen_primsc.sh \
                    runtime/primitives $(runtime_BYTECODE_C_SOURCES) \
                    > $@

$(SAK): runtime/sak.c runtime/caml/misc.h runtime/caml/config.h
	$(V_MKEXE)$(call SAK_BUILD,$@,$<)

C_LITERAL = $(shell $(SAK) $(ENCODE_C_LITERAL) $(call QUOTE_SINGLE,$(1)))

runtime/build_config.h: $(ROOTDIR)/Makefile.config \
                        $(ROOTDIR)/Makefile.build_config $(SAK)
	$(V_GEN){ \
	  echo '/* This file is generated from $(ROOTDIR)/Makefile.config */'; \
	  printf '#define OCAML_STDLIB_DIR %s\n' \
	         $(call QUOTE_SINGLE,$(call C_LITERAL,$(TARGET_LIBDIR))); \
	  echo '#define HOST "$(HOST)"'; \
	  echo '#define BYTECODE_RUNTIME_ID "$(BYTECODE_RUNTIME_ID)"'; \
	} > $@

runtime/prims.$(O): runtime/build_config.h

## Runtime libraries and programs

runtime/ocamlrun$(EXE): runtime/prims.$(O) runtime/libcamlrun.$(A)
	$(V_MKEXE)$(MKEXE) -o $@ $^ $(BYTECCLIBS)

runtime/ocamlruns$(EXE): runtime/prims.$(O) runtime/libcamlrun_non_shared.$(A)
	$(V_MKEXE)$(call MKEXE_VIA_CC,$@,$^ $(BYTECCLIBS))

runtime/libcamlrun.$(A): $(libcamlrun_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libcamlrun_non_shared.$(A): $(libcamlrun_non_shared_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/ocamlrund$(EXE): runtime/prims.$(O) runtime/libcamlrund.$(A)
	$(V_MKEXE)$(MKEXE) $(MKEXEDEBUGFLAG) -o $@ $^ $(BYTECCLIBS)

runtime/libcamlrund.$(A): $(libcamlrund_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/ocamlruni$(EXE): runtime/prims.$(O) runtime/libcamlruni.$(A)
	$(V_MKEXE)$(MKEXE) -o $@ $^ $(INSTRUMENTED_RUNTIME_LIBS) $(BYTECCLIBS)

runtime/libcamlruni.$(A): $(libcamlruni_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libcamlrun_pic.$(A): $(libcamlrunpic_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libcamlrun_shared.$(SO): $(libcamlrunpic_OBJECTS)
	$(V_MKDLL)$(MKDLL) -o $@ $^ $(BYTECCLIBS)

runtime/libasmrun.$(A): $(libasmrun_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmrund.$(A): $(libasmrund_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmruni.$(A): $(libasmruni_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmrun_pic.$(A): $(libasmrunpic_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

runtime/libasmrun_shared.$(SO): $(libasmrunpic_OBJECTS)
	$(V_MKDLL)$(MKDLL) -o $@ $^ $(NATIVECCLIBS)

runtime/libcomprmarsh.$(A): $(libcomprmarsh_OBJECTS)
	$(V_MKLIB)$(call MKLIB,$@, $^)

## Runtime target-specific preprocessor and compiler flags

runtime/%.b.$(O): OC_CFLAGS = $(OC_BYTECODE_CFLAGS)
runtime/%.b.$(O): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrun_CPPFLAGS)
$(DEPDIR)/runtime/%.b.$(D): \
  OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrun_CPPFLAGS)

runtime/%.bd.$(O): OC_CFLAGS = $(OC_BYTECODE_CFLAGS)
runtime/%.bd.$(O): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrund_CPPFLAGS)
$(DEPDIR)/runtime/%.bd.$(D): \
  OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrund_CPPFLAGS)

runtime/%.bi.$(O): OC_CFLAGS = $(OC_BYTECODE_CFLAGS)
runtime/%.bi.$(O): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlruni_CPPFLAGS)
$(DEPDIR)/runtime/%.bi.$(D): \
  OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlruni_CPPFLAGS)

runtime/%.bpic.$(O): OC_CFLAGS = $(OC_BYTECODE_CFLAGS) $(SHAREDLIB_CFLAGS)
runtime/%.bpic.$(O): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrun_CPPFLAGS)
$(DEPDIR)/runtime/%.bpic.$(D): \
  OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrun_CPPFLAGS)

runtime/%.n.$(O): OC_CFLAGS = $(OC_NATIVE_CFLAGS)
runtime/%.n.$(O): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrun_CPPFLAGS)
$(DEPDIR)/runtime/%.n.$(D): \
  OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrun_CPPFLAGS)

runtime/%.nd.$(O): OC_CFLAGS = $(OC_NATIVE_CFLAGS)
runtime/%.nd.$(O): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrund_CPPFLAGS)
$(DEPDIR)/runtime/%.nd.$(D): \
  OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrund_CPPFLAGS)

runtime/%.ni.$(O): OC_CFLAGS = $(OC_NATIVE_CFLAGS)
runtime/%.ni.$(O): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlruni_CPPFLAGS)
$(DEPDIR)/runtime/%.ni.$(D): \
  OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlruni_CPPFLAGS)

runtime/%.npic.$(O): OC_CFLAGS = $(OC_NATIVE_CFLAGS) $(SHAREDLIB_CFLAGS)
runtime/%.npic.$(O): OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrun_CPPFLAGS)
$(DEPDIR)/runtime/%.npic.$(D): \
  OC_CPPFLAGS = $(OC_NATIVE_CPPFLAGS) $(ocamlrun_CPPFLAGS)

## Compilation of C files

# There are two scenarios in which a C object may need to be rebuilt:
# 1. The C source file is newer than the object
# 2. A file #include'd by the C source file is newer than the object
#
# When the C source file is newer than the object, we do not have to care about
# which included files are newer, we just have to be sure that all the files
# which may be #include'd definitely exist.
#
# GCC and clang can both generate the precise dependency information
# needed for (2) as part of compiling the C file. We therefore use C dependency
# information lazily.

# All C files must depend on the _presence_ of the $(runtime_BUILT_HEADERS). If
# a C file actually uses one of these headers, then the dependency will be
# recorded in the .d file (and becomes a real dependency, not an order-only
# dependency).
runtime_MISSING_BUILT_HEADERS = \
  $(filter-out $(wildcard $(runtime_BUILT_HEADERS)), $(runtime_BUILT_HEADERS))

# COMPILE_C_FILE generates the compilation rules for C objects
#   $1 = target pattern for the generated object file (without the .$(O))
#   $2 = source pattern for the C source file (without the .c)
#   $3 = optional; if non-empty suppresses the generation of dependency rules
define DO_COMPILE_C_FILE
ifeq "$(COMPUTE_DEPS)" "true"
# Secondary expansion means we can use @D to depend on the directory being
# created.
$(1).$(O): $(2).c \
  $(if $(3),,| $(DEPDIR)/$$$$(@D) $(runtime_MISSING_BUILT_HEADERS))
else
$(1).$(O): $(2).c \
  $(runtime_CONFIGURED_HEADERS) $(runtime_BUILT_HEADERS) \
  $(RUNTIME_HEADERS)
endif # ifeq "$(COMPUTE_DEPS)" "true"
	$$(V_CC)$$(CC) $$(OC_CFLAGS) $$(CFLAGS) $$(OC_CPPFLAGS) $$(CPPFLAGS) \
	  $$(OUTPUTOBJ)$$@ \
	  $(if $(3),,$(call DEP_FLAGS,$$@,$(DEPDIR)/$$(@:.$(O)=.$(D)))) \
	  -c $$<
# This is skipped if either $(COMPUTE_DEPS) is false or $(3) is non-empty
ifeq "$(COMPUTE_DEPS)$(3)" "true"
# MSVC doesn't emit usable dependency information, but if GCC is available
# then it can instead be called in order to generate the .d file.
ifneq "$(CC)" "$(DEP_CC)"
	$$(V_CCDEPS)$$(DEP_CC) $(DEP_CPPFLAGS) $$(OC_CPPFLAGS) $$(CPPFLAGS) $$< \
   -MM -MT $$@ -MF $(DEPDIR)/$$(@:.$(O)=.$(D))
endif # ifneq "$(CC)" "$(DEP_CC)"
endif # ifeq "$(COMPUTE_DEPS)$(3)" "true"
endef

# The additional call expands the optional $(3) to an empty string
COMPILE_C_FILE = \
  $(call DO_COMPILE_C_FILE,$(1),$(2),$\
                           $(if $(filter-out undefined,$(origin 3)),$(3)))

.PRECIOUS: $(DEPDIR)/%
$(DEPDIR)/%:
	$(MKDIR) $@

runtime_OBJECT_TYPES = % %.b %.bd %.bi %.bpic
ifeq "$(NATIVE_COMPILER)" "true"
runtime_OBJECT_TYPES += %.n %.nd %.ni %.np %.npic
endif

$(foreach runtime_OBJECT_TYPE, $(runtime_OBJECT_TYPES), \
  $(eval $(call COMPILE_C_FILE,runtime/$(runtime_OBJECT_TYPE),runtime/%)))

runtime/$(UNIX_OR_WIN32)_non_shared.%.$(O): \
  OC_CPPFLAGS += -DBUILDING_LIBCAMLRUNS

$(eval $(call COMPILE_C_FILE,runtime/$(UNIX_OR_WIN32)_non_shared.%, \
  runtime/$(UNIX_OR_WIN32)))

$(foreach runtime_OBJECT_TYPE,$(subst %,,$(runtime_OBJECT_TYPES)), \
  $(eval \
    runtime/dynlink$(runtime_OBJECT_TYPE).$(O): $(ROOTDIR)/Makefile.config))

$(eval $(call COMPILE_C_FILE,yacc/%,yacc/%,no-deps))

## Compilation of runtime assembly files

ASPP_ERROR = \
  { echo "If your assembler produced syntax errors, it is probably";\
          echo "unhappy with the preprocessor. Check your assembler, or";\
          echo "try producing $*.o by hand.";\
          exit 2; }
runtime/%.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%.d.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) $(ocamlrund_CPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%.i.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) $(ocamlruni_CPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%_libasmrunpic.o: runtime/%.S
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) $(SHAREDLIB_CFLAGS) -o $@ $<

runtime/domain_state.inc: runtime/caml/domain_state.tbl
	$(V_GEN)$(CPP) $< > $@

runtime/amd64nt.obj: runtime/amd64nt.asm runtime/domain_state.inc
	$(V_ASM)$(ASM)$@ $<

runtime/amd64nt.d.obj: runtime/amd64nt.asm runtime/domain_state.inc
	$(V_ASM)$(ASM)$@ $(ocamlrund_CPPFLAGS) $<

runtime/amd64nt.i.obj: runtime/amd64nt.asm runtime/domain_state.inc
	$(V_ASM)$(ASM)$@ $(ocamlruni_CPPFLAGS) $<

runtime/%_libasmrunpic.obj: runtime/%.asm
	$(V_ASM)$(ASM)$@ $<

## Runtime dependencies

RUNTIME_DEP_FILES := $(wildcard $(DEPDIR)/runtime/*.$(D))
.PHONY: $(RUNTIME_DEP_FILES)
include $(RUNTIME_DEP_FILES)

.PHONY: runtime
runtime: stdlib/libcamlrun.$(A)

.PHONY: makeruntime
makeruntime: runtime-all
stdlib/libcamlrun.$(A): runtime-all
	cd stdlib; $(LN) ../runtime/libcamlrun.$(A) .
clean::
	rm -f $(addprefix runtime/, *.o *.obj *.a *.lib *.so *.dll)
	rm -f $(addprefix runtime/, ocamlrun ocamlrund ocamlruni ocamlruns sak)
	rm -f $(addprefix runtime/, \
	  ocamlrun.exe ocamlrund.exe ocamlruni.exe ocamlruns.exe sak.exe)
	rm -f runtime/primitives runtime/primitives*.new runtime/prims.c \
	  $(runtime_BUILT_HEADERS)
	rm -f runtime/domain_state.inc
	rm -rf $(DEPDIR)
	rm -f stdlib/libcamlrun.a stdlib/libcamlrun.lib

.PHONY: runtimeopt
runtimeopt: stdlib/libasmrun.$(A)

.PHONY: makeruntimeopt
makeruntimeopt: runtime-allopt
stdlib/libasmrun.$(A): runtime-allopt
	cd stdlib; $(LN) ../runtime/libasmrun.$(A) .
stdlib/libcomprmarsh.$(A): runtime/libcomprmarsh.$(A)
	cd stdlib; $(LN) ../runtime/libcomprmarsh.$(A) .

clean::
	rm -f stdlib/libasmrun.a stdlib/libasmrun.lib
	rm -f stdlib/libcomprmarsh.a stdlib/libcomprmarsh.lib

# Dependencies

# The following definition duplicates the otherlibs/ prefix but this
# will go away with the merge of the sub makefiles
subdirs = stdlib \
  otherlibs/str \
  otherlibs/systhreads \
  otherlibs/unix \
  otherlibs/runtime_events

.PHONY: alldepend
alldepend: depend
	for dir in $(subdirs); do \
	  $(MAKE) -C $$dir depend || exit; \
	done

# The standard library

.PHONY: library
library: ocamlc
	$(MAKE) -C stdlib all

.PHONY: library-cross
library-cross:
	$(MAKE) -C stdlib OCAMLRUN=../runtime/ocamlrun$(EXE) all

.PHONY: libraryopt
libraryopt:
	$(MAKE) -C stdlib allopt

partialclean::
	$(MAKE) -C stdlib clean

# The lexer generator

ocamllex_LIBRARIES =

ocamllex_SOURCES = $(addprefix lex/,\
  cset.mli cset.ml \
  syntax.mli syntax.ml \
  parser.mly \
  lexer.mli lexer.mll \
  table.mli table.ml \
  lexgen.mli lexgen.ml \
  compact.mli compact.ml \
  common.mli common.ml \
  output.mli output.ml \
  outputbis.mli outputbis.ml \
  main.mli main.ml)

.PHONY: lex-all
lex-all: lex/ocamllex

.PHONY: lex-allopt
lex-allopt: lex/ocamllex.opt

.PHONY: ocamllex
ocamllex: ocamlyacc
	$(MAKE) lex-all

.PHONY: ocamllex.opt
ocamllex.opt: ocamlopt
	$(MAKE) lex-allopt

ocamllex_BYTECODE_LINKFLAGS = -compat-32

ifeq "$(IN_COREBOOT_CYCLE)" "true"
ocamllex_BYTECODE_LINKFLAGS += -set-runtime-default standard_library_default=.
endif

partialclean::
	rm -f lex/*.cm* lex/*.o lex/*.obj \
        $(ocamllex_PROGRAMS) $(ocamllex_PROGRAMS:=.exe) \
        lex/parser.ml lex/parser.mli lex/parser.output \
        lex/lexer.ml

beforedepend:: lex/parser.ml lex/parser.mli lex/lexer.ml

# The ocamlyacc parser generator

ocamlyacc_OTHER_MODULES = $(addprefix yacc/,\
  closure error lalr lr0 main mkpar output reader skeleton symtab \
  verbose warshall)

ocamlyacc_MODULES = $(ocamlyacc_WSTR_MODULE) $(ocamlyacc_OTHER_MODULES)

ocamlyacc_OBJECTS = $(ocamlyacc_MODULES:=.$(O))

# Do not compile assertions in ocamlyacc
ocamlyacc_CPPFLAGS = -DNDEBUG

.PHONY: ocamlyacc
ocamlyacc: $(ocamlyacc_PROGRAM)$(EXE)

$(ocamlyacc_PROGRAM)$(EXE): $(ocamlyacc_OBJECTS)
	$(V_MKEXE)$(MKEXE) -o $@ $^

clean::
	rm -f $(ocamlyacc_MODULES:=.o) $(ocamlyacc_MODULES:=.obj) \
        yacc/wstr.o yacc/wstr.obj

$(ocamlyacc_OTHER_MODULES:=.$(O)): yacc/defs.h

$(ocamlyacc_OTHER_MODULES:=.$(O)): OC_CPPFLAGS += $(ocamlyacc_CPPFLAGS)

# The Menhir-generated parser

# In order to avoid a build-time dependency on Menhir,
# we store the result of the parser generator (which
# are OCaml source files) and Menhir's runtime libraries
# (that the parser files rely on) in boot/.

# The rules below do not depend on Menhir being available,
# they just build the parser from boot/.

# See Makefile.menhir for the rules to rebuild the parser and update
# boot/, which require Menhir. The targets in Makefile.menhir
# (also included here for convenience) must be used after any
# modification of parser.mly.
include Makefile.menhir

# To avoid module-name conflicts with compiler-lib users that link
# with their code with their own MenhirLib module (possibly with
# a different Menhir version), we rename MenhirLib into
# CamlinternalMenhirlib -- and replace the module occurrences in the
# generated parser.ml.

parsing/camlinternalMenhirLib.ml: boot/menhir/menhirLib.ml
	$(V_GEN)cp $< $@
parsing/camlinternalMenhirLib.mli: boot/menhir/menhirLib.mli
	$(V_GEN)echo '[@@@ocaml.warning "-67"]' > $@ && \
	cat $< >> $@

# Copy parsing/parser.ml from boot/

PARSER_DEPS = boot/menhir/parser.ml parsing/parser.mly

ifeq "$(OCAML_DEVELOPMENT_VERSION)" "true"
PARSER_DEPS += tools/check-parser-uptodate-or-warn.sh
endif

parsing/parser.ml: $(PARSER_DEPS)
ifeq "$(OCAML_DEVELOPMENT_VERSION)" "true"
	@-tools/check-parser-uptodate-or-warn.sh
endif
	$(V_GEN)sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@
parsing/parser.mli: boot/menhir/parser.mli
	$(V_GEN)sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@

beforedepend:: parsing/camlinternalMenhirLib.ml \
  parsing/camlinternalMenhirLib.mli \
  parsing/parser.ml parsing/parser.mli

partialclean:: partialclean-menhir


# OCamldoc

# First define the odoc_info library used to build OCamldoc

odoc_info_SOURCES = $(addprefix ocamldoc/,\
  odoc_config.mli odoc_config.ml \
  odoc_messages.mli odoc_messages.ml \
  odoc_global.mli odoc_global.ml \
  odoc_types.mli odoc_types.ml \
  odoc_misc.mli odoc_misc.ml \
  odoc_text_parser.mly \
  odoc_text_lexer.mli odoc_text_lexer.mll \
  odoc_text.mli odoc_text.ml \
  odoc_name.mli odoc_name.ml \
  odoc_parameter.mli odoc_parameter.ml \
  odoc_value.mli odoc_value.ml \
  odoc_type.mli odoc_type.ml \
  odoc_extension.mli odoc_extension.ml \
  odoc_exception.mli odoc_exception.ml \
  odoc_class.mli odoc_class.ml \
  odoc_module.mli odoc_module.ml \
  odoc_print.mli odoc_print.ml \
  odoc_str.mli odoc_str.ml \
  odoc_comments_global.mli odoc_comments_global.ml \
  odoc_parser.mly \
  odoc_lexer.mli odoc_lexer.mll \
  odoc_see_lexer.mli odoc_see_lexer.mll \
  odoc_env.mli odoc_env.ml \
  odoc_merge.mli odoc_merge.ml \
  odoc_sig.mli odoc_sig.ml \
  odoc_ast.mli odoc_ast.ml \
  odoc_search.mli odoc_search.ml \
  odoc_scan.mli odoc_scan.ml \
  odoc_cross.mli odoc_cross.ml \
  odoc_comments.mli odoc_comments.ml \
  odoc_dep.mli odoc_dep.ml \
  odoc_analyse.mli odoc_analyse.ml \
  odoc_info.mli odoc_info.ml)

ocamldoc_LIBRARIES = \
  compilerlibs/ocamlcommon \
  $(addprefix otherlibs/,\
    unix/unix \
    str/str \
    dynlink/dynlink) \
  ocamldoc/odoc_info

ocamldoc_SOURCES = $(addprefix ocamldoc/,\
  odoc_dag2html.mli odoc_dag2html.ml \
  odoc_to_text.mli odoc_to_text.ml \
  odoc_ocamlhtml.mli odoc_ocamlhtml.mll \
  odoc_html.mli odoc_html.ml \
  odoc_man.mli odoc_man.ml \
  odoc_latex_style.mli odoc_latex_style.ml \
  odoc_latex.mli odoc_latex.ml \
  odoc_texi.mli odoc_texi.ml \
  odoc_dot.mli odoc_dot.ml \
  odoc_gen.mli odoc_gen.ml \
  odoc_args.mli odoc_args.ml \
  odoc.mli odoc.ml)

# OCamldoc files to install (a subset of what is built)

OCAMLDOC_LIBMLIS = $(addprefix ocamldoc/,$(addsuffix .mli,\
  odoc_dep odoc_dot odoc_extension odoc_html odoc_info odoc_latex \
  odoc_latex_style odoc_man odoc_messages odoc_ocamlhtml odoc_parameter \
  odoc_texi odoc_text_lexer odoc_to_text odoc_type odoc_value))
OCAMLDOC_LIBCMIS=$(OCAMLDOC_LIBMLIS:.mli=.cmi)
OCAMLDOC_LIBCMTS=$(OCAMLDOC_LIBMLIS:.mli=.cmt) $(OCAMLDOC_LIBMLIS:.mli=.cmti)

ocamldoc/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)
ocamldoc/%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)

ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "false"
# ocamldoc needs a custom runtime when building statically owing to the C stubs
# in unix.cma and str.cma. This is specified explicitly to suppress the default
# linking flags (see $(MAYBE_ADD_BYTECODE_LAUNCHER_FLAGS) in Makefile.common)
ocamldoc/ocamldoc$(EXE): ocamldoc_BYTECODE_LINKFLAGS += -custom
endif

.PHONY: ocamldoc
ocamldoc: ocamldoc/ocamldoc$(EXE) ocamldoc/odoc_test.cmo \
  ocamlc ocamlyacc ocamllex

.PHONY: ocamldoc.opt
ocamldoc.opt: ocamldoc/ocamldoc.opt$(EXE) ocamlopt ocamlyacc ocamllex

# OCamltest

# Libraries ocamltest depends on

ocamltest_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp) \
  $(unix_library)

# List of source files from which ocamltest is compiled
# (all the different sorts of files are derived from this)

# ocamltest has two components: its core and the OCaml "plugin"
# which is actually built into the tool but clearly separated from its core

ocamltest_CORE = \
  run_$(UNIX_OR_WIN32).c run_stubs.c \
  ocamltest_config.ml.in ocamltest_config.mli \
  ocamltest_unix.mli ocamltest_unix.ml \
  ocamltest_stdlib.mli ocamltest_stdlib.ml \
  run_command.mli run_command.ml \
  filecompare.mli filecompare.ml \
  variables.mli variables.ml \
  environments.mli environments.ml \
  result.mli result.ml \
  actions.mli actions.ml \
  tests.mli tests.ml \
  strace.mli strace.ml \
  tsl_ast.mli tsl_ast.ml \
  tsl_parser.mly \
  tsl_lexer.mli tsl_lexer.mll \
  modifier_parser.mli modifier_parser.ml \
  tsl_semantics.mli tsl_semantics.ml \
  builtin_variables.mli builtin_variables.ml \
  actions_helpers.mli actions_helpers.ml \
  builtin_actions.mli builtin_actions.ml \
  translate.mli translate.ml

ocamltest_ocaml_PLUGIN = \
  ocaml_backends.mli ocaml_backends.ml \
  ocaml_filetypes.mli ocaml_filetypes.ml \
  ocaml_variables.mli ocaml_variables.ml \
  ocaml_modifiers.mli ocaml_modifiers.ml \
  ocaml_directories.mli ocaml_directories.ml \
  ocaml_files.mli ocaml_files.ml \
  ocaml_flags.mli ocaml_flags.ml \
  ocaml_commands.mli ocaml_commands.ml \
  ocaml_tools.mli ocaml_tools.ml \
  ocaml_compilers.mli ocaml_compilers.ml \
  ocaml_toplevels.mli ocaml_toplevels.ml \
  ocaml_actions.mli ocaml_actions.ml \
  ocaml_tests.mli ocaml_tests.ml \
  debugger_flags.mli debugger_flags.ml \
  debugger_variables.mli debugger_variables.ml \
  debugger_actions.mli debugger_actions.ml \

ocamltest_SOURCES = $(addprefix ocamltest/, \
  $(ocamltest_CORE) $(ocamltest_ocaml_PLUGIN) \
  options.mli options.ml \
  main.mli main.ml)

$(eval $(call COMPILE_C_FILE,ocamltest/%.b,ocamltest/%))
$(eval $(call COMPILE_C_FILE,ocamltest/%.n,ocamltest/%))

ocamltest_DEPEND_FILES := $(wildcard $(DEPDIR)/ocamltest/*.$(D))
.PHONY: $(ocamltest_DEPEND_FILES)
include $(ocamltest_DEPEND_FILES)

ocamltest/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)

ocamltest/%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)

ocamltest: ocamltest/ocamltest$(EXE) \
  testsuite/lib/lib.cmo testsuite/lib/testing.cma testsuite/tools/expect$(EXE) \
  ocamlc ocamlyacc ocamllex

testsuite/lib/%: VPATH += testsuite/lib

testing_SOURCES = testsuite/lib/testing.mli testsuite/lib/testing.ml
testing_LIBRARIES =

$(addprefix testsuite/lib/testing., cma cmxa): \
  OC_COMMON_LINKFLAGS += -linkall

testsuite/tools/%: VPATH += testsuite/tools

expect_SOURCES = $(addprefix testsuite/tools/,expect.mli expect.ml)
expect_LIBRARIES = $(addprefix compilerlibs/,\
  ocamlcommon ocamlbytecomp ocamltoplevel)

expect_BYTECODE_LINKFLAGS += -linkall

codegen_SOURCES = $(addprefix testsuite/tools/,\
  parsecmmaux.mli parsecmmaux.ml \
  parsecmm.mly \
  lexcmm.mli lexcmm.mll \
  codegen_main.mli codegen_main.ml)
codegen_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamloptcomp)

# The asmgen tests are not ported to MSVC64 yet, so make sure
# to compile the arch specific module they require only if necessary
ifeq "$(CCOMPTYPE)-$(ARCH)" "msvc-amd64"
asmgen_OBJECT =
else
asmgen_MODULE = testsuite/tools/asmgen_$(ARCH)
asmgen_SOURCE = $(asmgen_MODULE).S
asmgen_OBJECT = $(asmgen_MODULE).$(O)
$(asmgen_OBJECT): $(asmgen_SOURCE)
	$(V_ASM)$(ASPP) $(OC_ASPPFLAGS) -o $@ $< || $(ASPP_ERROR)
endif

test_in_prefix_SOURCES = $(addprefix testsuite/tools/,\
  toolchain.mli toolchain.ml \
  harness.mli harness.ml \
  environment.mli environment.ml \
  cmdline.mli cmdline.ml \
  testBytecodeBinaries.mli testBytecodeBinaries.ml \
  testDynlink.mli testDynlink.ml \
  testLinkModes.mli testLinkModes.ml \
  testRelocation.mli testRelocation.ml \
  testToplevel.mli testToplevel.ml \
  test_ld_conf.mli test_ld_conf.ml \
  test_in_prefix.mli test_in_prefix.ml)
test_in_prefix_LIBRARIES = \
  otherlibs/unix/unix compilerlibs/ocamlcommon compilerlibs/ocamlbytecomp

# test_in_prefix% would only match test_in_prefix.opt, hence the missing 'x'!
testsuite/tools/test_in_prefi%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)

test_in_prefix_BYTECODE_LINKFLAGS += -custom

ifeq "$(TARGET_LIBDIR_IS_RELATIVE)" "true"
# testsuite/tools/test_in_prefix cannot use a relative stdlib because it is run
# from testsuite/tools, not from the installation tree (the alternative would be
# to compile it directly with the installed compiler)
test_in_prefix_NATIVE_LINKFLAGS =
test_in_prefix_COMMON_LINKFLAGS = \
  -set-runtime-default 'standard_library_default=$(LIBDIR)'
endif

testsuite/tools/test_in_prefi%: CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)

testsuite/tools/poisonedruntime$(EXE): testsuite/tools/poisonedruntime.$(O)
	$(V_MKEXE)$(call MKEXE_VIA_CC,$@,$^)

$(eval $(call COMPILE_C_FILE,\
  testsuite/tools/main_in_c,testsuite/tools/main_in_c,no-deps))
$(eval $(call COMPILE_C_FILE,\
  testsuite/tools/poisonedruntime,testsuite/tools/poisonedruntime,no-deps))

ocamltest_BYTECODE_LINKFLAGS = -custom -g

ocamltest/ocamltest$(EXE): ocamlc ocamlyacc ocamllex

ocamltest/ocamltest.opt$(EXE): ocamlopt ocamlyacc ocamllex

# ocamltest does _not_ want to have access to the Unix interface by default,
# to ensure functions and types are only used via Ocamltest_stdlib.Unix
# (see #9797)
ocamltest/%: \
  VPATH := $(filter-out $(unix_directory), $(VPATH))

# Ocamltest_unix and the linking of the executable itself should include the
# Unix library, if it's being built.
ocamltest/ocamltest_unix.% \
ocamltest/ocamltest$(EXE) ocamltest/ocamltest.opt$(EXE): \
  VPATH += $(unix_directory)

# For flambda mode, it is necessary for Ocamltest_unix to be compiled with
# -opaque to prevent errors compiling the other modules of ocamltest.
ocamltest/ocamltest_unix.%: \
  OC_COMMON_COMPFLAGS += -opaque
ifeq "$(build_ocamltest)" "true"
ocamltest: ocamltest/ocamltest$(EXE) \
  testsuite/lib/lib.cmo testsuite/lib/testing.cma testsuite/tools/expect$(EXE) \
  ocamlc ocamlyacc ocamllex

ocamltest.opt: ocamltest/ocamltest.opt$(EXE) \
  testsuite/lib/testing.cmxa $(asmgen_OBJECT) testsuite/tools/codegen$(EXE) \
  ocamlopt ocamlyacc ocamllex
else # ifeq "$(build_ocamltest)" "true"
ocamltest_TARGETS = ocamltest ocamltest.opt
.PHONY: $(ocamltest_TARGETS)
$(ocamltest_TARGETS):
	@echo ocamltest is disabled
	@echo To build it, run configure again with --enable-ocamltest
	@false
endif # ifeq "$(build_ocamltest)" "true"

partialclean::
	rm -f ocamltest/ocamltest ocamltest/ocamltest.exe
	rm -f ocamltest/ocamltest.opt ocamltest/ocamltest.opt.exe
	rm -f $(addprefix ocamltest/,*.o *.obj *.cm*)
	rm -f $(patsubst %.mll,%.ml, $(wildcard ocamltest/*.mll))
	rm -f $(patsubst %.mly,%.ml, $(wildcard ocamltest/*.mly))
	rm -f $(patsubst %.mly,%.mli, $(wildcard ocamltest/*.mly))
	rm -f $(patsubst %.mly,%.output, $(wildcard ocamltest/*.mly))
	rm -f ocamltest/ocamltest.html
	rm -f $(addprefix testsuite/lib/*.,cm* o obj a lib)
	rm -f $(addprefix testsuite/tools/*.,cm* o obj a lib)
	rm -f testsuite/tools/codegen testsuite/tools/codegen.exe
	rm -f testsuite/tools/poisonedruntime testsuite/tools/poisonedruntime.exe
	rm -f testsuite/tools/expect testsuite/tools/expect.exe
	rm -f testsuite/tools/test_in_prefix testsuite/tools/test_in_prefix.exe
	rm -f testsuite/tools/test_in_prefix.opt \
        testsuite/tools/test_in_prefix.opt.exe
	rm -f testsuite/tools/lexcmm.ml
	rm -f $(addprefix testsuite/tools/parsecmm., ml mli output)

ocamltest/ocamltest_config.ml ocamltest/ocamltest_unix.ml: config.status
	./$< $@

beforedepend:: ocamltest/ocamltest_config.ml ocamltest/ocamltest_unix.ml

# Documentation

.PHONY: html_doc
html_doc: ocamldoc
	$(MAKE) -C api_docgen html

.PHONY: manpages
manpages:
	$(MAKE) -C api_docgen man

partialclean::
	rm -f ocamldoc/ocamldoc ocamldoc/ocamldoc.exe
	rm -f ocamldoc/ocamldoc.opt ocamldoc/ocamldoc.opt.exe
	rm -f ocamldoc/\#*\#
	rm -f ocamldoc/*.cm[aiotx] ocamldoc/*.cmxa ocamldoc/*.cmti \
	  ocamldoc/*.a ocamldoc/*.lib ocamldoc/*.o ocamldoc/*.obj
	rm -f ocamldoc/odoc_parser.output ocamldoc/odoc_text_parser.output
	rm -f ocamldoc/odoc_lexer.ml ocamldoc/odoc_text_lexer.ml \
	  ocamldoc/odoc_see_lexer.ml ocamldoc/odoc_ocamlhtml.ml
	rm -f ocamldoc/odoc_parser.ml ocamldoc/odoc_parser.mli \
	  ocamldoc/odoc_text_parser.ml ocamldoc/odoc_text_parser.mli

partialclean::
	$(MAKE) -C api_docgen clean

# The OCamltest manual

.PHONY: ocamltest-manual
ocamltest-manual: ocamltest/ocamltest.html

ocamltest/ocamltest.html: ocamltest/OCAMLTEST.org
	pandoc -s --toc -N -f org -t html -o $@ $<

# The extra libraries

.PHONY: otherlibraries
otherlibraries: ocamltools dynlink-all
	$(MAKE) -C otherlibs all

.PHONY: otherlibrariesopt
otherlibrariesopt: dynlink-allopt
	$(MAKE) -C otherlibs allopt

otherlibs/unix/unix.cmxa: otherlibrariesopt
otherlibs/str/str.cmxa: otherlibrariesopt

partialclean::
	rm -f otherlibs/dynlink/*.cm[ioaxt] otherlibs/dynlink/*.cmti \
	  otherlibs/dynlink/*.cmxa otherlibs/dynlink/byte/*.cm[iot] \
	  otherlibs/dynlink/byte/*.cmti otherlibs/dynlink/native/*.cm[ixt] \
	  otherlibs/dynlink/native/*.cmti otherlibs/dynlink/native/*.o \
	  otherlibs/dynlink/native/*.obj
	$(MAKE) -C otherlibs partialclean

clean::
	rm -f otherlibs/dynlink/*.a otherlibs/dynlink/*.lib \
	  otherlibs/dynlink/*.o otherlibs/dynlink/*.obj \
	  otherlibs/dynlink/*.so otherlibs/dynlink/*.dll \
	  otherlibs/dynlink/byte/dynlink.mli \
	  otherlibs/dynlink/native/dynlink.mli \

	$(MAKE) -C otherlibs clean

# The replay debugger

ocamldebug_LIBRARIES = compilerlibs/ocamlcommon \
  $(addprefix otherlibs/,unix/unix dynlink/dynlink)

# The following dependencies are necessary at the moment, because the
# root Makefile does not know yet how to build the other libraries
# Once their build will happen in this root Makefile, too, it will become
# possible to get rid of these dependencies

otherlibs/unix/unix.cma: otherlibraries
otherlibs/str/str.cma: otherlibraries

debugger/%: VPATH += otherlibs/unix otherlibs/dynlink

ocamldebug_COMPILER_SOURCES = $(addprefix toplevel/, \
  genprintval.mli genprintval.ml \
  topprinters.mli topprinters.ml)

# The modules listed in the following variable are packed into ocamldebug.cmo

ocamldebug_DEBUGGER_SOURCES = $(addprefix debugger/,\
  int64ops.mli int64ops.ml \
  primitives.mli primitives.ml \
  unix_tools.mli unix_tools.ml \
  debugger_config.mli debugger_config.ml \
  parameters.mli parameters.ml \
  debugger_lexer.mli debugger_lexer.mll \
  input_handling.mli input_handling.ml \
  question.mli question.ml \
  debugcom.mli debugcom.ml \
  exec.mli exec.ml \
  source.mli source.ml \
  pos.mli pos.ml \
  checkpoints.mli checkpoints.ml \
  events.mli events.ml \
  program_loading.mli program_loading.ml \
  symbols.mli symbols.ml \
  breakpoints.mli breakpoints.ml \
  trap_barrier.mli trap_barrier.ml \
  history.mli history.ml \
  printval.mli printval.ml \
  show_source.mli show_source.ml \
  time_travel.mli time_travel.ml \
  program_management.mli program_management.ml \
  frames.mli frames.ml \
  eval.mli eval.ml \
  show_information.mli show_information.ml \
  loadprinter.mli loadprinter.ml \
  debugger_parser.mly \
  command_line.mli command_line.ml \
  main.mli main.ml)

ocamldebug_DEBUGGER_OBJECTS = \
  $(patsubst %.ml, %.cmo, \
    $(patsubst %.mll, %.cmo, \
      $(patsubst %.mly, %.cmo, \
        $(filter-out %.mli, $(ocamldebug_DEBUGGER_SOURCES)))))

ocamldebug_SOURCES = \
  $(ocamldebug_COMPILER_SOURCES) \
  $(addprefix debugger/, \
    ocamldebug.ml \
    ocamldebug_entry.mli ocamldebug_entry.ml)

ocamldebug_BYTECODE_LINKFLAGS = -linkall

debugger/%: CAMLC = $(BEST_OCAMLC) $(STDLIBFLAGS)

.PHONY: ocamldebug ocamldebugger
ocamldebug: debugger/ocamldebug$(EXE) ocamlc ocamlyacc ocamllex
ocamldebugger: debugger/ocamldebug$(EXE) ocamlc ocamlyacc ocamllex
# the 'ocamldebugger' target is an alias of 'ocamldebug' for
# backward-compatibility with old ./configure scripts; it can be
# removed after most contributors have re-run ./configure once, for
# example after 5.2 is branched

$(ocamldebug_DEBUGGER_OBJECTS): OC_COMMON_COMPFLAGS += -for-pack ocamldebug
debugger/ocamldebug.cmo: $(ocamldebug_DEBUGGER_OBJECTS)
	$(V_OCAMLC)$(CAMLC) $(OC_COMMON_COMPFLAGS) -pack -o $@ $^

debugger/ocamldebug_entry.cmo: debugger/ocamldebug.cmo

ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "false"
# ocamldebug needs a custom runtime when building statically owing to the
# C stubs in unix.cma. This is specified explicitly to suppress the default
# linking flags (see $(MAYBE_ADD_BYTECODE_LAUNCHER_FLAGS) in Makefile.common)
debugger/ocamldebug$(EXE): ocamldebug_BYTECODE_LINKFLAGS += -custom
endif

clean::
	rm -f debugger/ocamldebug debugger/ocamldebug.exe
	rm -f debugger/debugger_lexer.ml
	rm -f $(addprefix debugger/debugger_parser.,ml mli output)

beforedepend:: debugger/debugger_lexer.ml

beforedepend:: debugger/debugger_parser.ml debugger/debugger_parser.mli

# Check that the native-code compiler is supported
.PHONY: checknative
checknative:
ifeq "$(NATIVE_COMPILER)" "false"
	$(error The source tree was configured with --disable-native-compiler!)
else
ifeq "$(ARCH)" "none"
	$(error The native-code compiler is not supported on this platform)
else
	@
endif
endif

# Check that the stack limit is reasonable (Unix-only)
$(eval $(call COMPILE_C_FILE,tools/checkstack,tools/checkstack,no-deps))
.PHONY: checkstack
ifeq "$(UNIX_OR_WIN32)" "unix"
checkstack: tools/checkstack$(EXE)
	$<

.INTERMEDIATE: tools/checkstack$(EXE) tools/checkstack.$(O)
tools/checkstack$(EXE): tools/checkstack.$(O)
	$(V_MKEXE)$(MKEXE) $(OUTPUTEXE)$@ $<
else
checkstack:
	@
endif

# Lint @since and @deprecated annotations

lintapidiff_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp) \
  otherlibs/str/str
lintapidiff_SOURCES = tools/lintapidiff.mli tools/lintapidiff.ml

tools/lintapidiff.opt$(EXE): VPATH += otherlibs/str

VERSIONS=$(shell git tag|grep '^[0-9]*.[0-9]*.[0-9]*$$'|grep -v '^[12].')
.PHONY: lintapidiff
lintapidiff: tools/lintapidiff.opt$(EXE)
	git ls-files -- 'otherlibs/*/*.mli' 'stdlib/*.mli' |\
	    grep -Ev internal\|obj\|stdLabels\|moreLabels |\
	    tools/lintapidiff.opt $(VERSIONS)

# Regenerate otherlibs/dynlink/byte/dynlink_symtable from its bytecomp sources

sync_dynlink_SOURCES = tools/sync_dynlink.mli tools/sync_dynlink.ml
sync_dynlink_LIBRARIES =

.PHONY: sync_dynlink
sync_dynlink: tools/sync_dynlink.opt$(EXE)
	    ./tools/sync_dynlink.opt$(EXE) \
        otherlibs/dynlink/byte/dynlink_symtable.ml \
      > synced_dynlink.tmp
	    diff -u synced_dynlink.tmp otherlibs/dynlink/byte/dynlink_symtable.ml
	    rm synced_dynlink.tmp
# Tools

TOOLS_BYTECODE_TARGETS = \
  $(TOOLS_NAT_PROGRAMS) $(TOOLS_BYT_PROGRAMS) $(TOOLS_MODULES:=.cmo)

TOOLS_NATIVE_TARGETS = $(TOOLS_MODULES:=.cmx)

TOOLS_OPT_TARGETS = $(TOOLS_NAT_PROGRAMS:=.opt)

.PHONY: ocamltools
ocamltools: ocamlc ocamllex
	$(MAKE) tools-all

.PHONY: tools-all
tools-all: $(TOOLS_BYTECODE_TARGETS)

.PHONY: tools-allopt
tools-allopt: $(TOOLS_NATIVE_TARGETS)

.PHONY: tools-allopt.opt
tools-allopt.opt: $(TOOLS_OPT_TARGETS)

.PHONY: ocamltoolsopt
ocamltoolsopt: ocamlopt
	$(MAKE) tools-allopt

.PHONY: ocamltoolsopt.opt
ocamltoolsopt.opt: ocamlc.opt ocamllex.opt
	$(MAKE) tools-allopt.opt

# Tools that require a full ocaml distribution: otherlibs and toplevel

OTHER_TOOLS =

ocamltex = tools/ocamltex$(EXE)

ifeq "$(build_ocamltex)" "true"
OTHER_TOOLS += $(ocamltex)
endif

.PHONY: othertools
othertools: $(OTHER_TOOLS)

partialclean::
	for prefix in cm* dll so lib a obj; do \
	  rm -f tools/*.$$prefix; \
	done

# The dependency generator

ocamldep_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
ocamldep_SOURCES = tools/ocamldep.mli tools/ocamldep.ml

ocamldep_BYTECODE_LINKFLAGS = -compat-32

# The profiler

ocamlprof_LIBRARIES =
ocamlprof_SOURCES = \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  format_doc.mli format_doc.ml \
  misc.mli misc.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  terminfo.mli terminfo.ml \
  warnings.mli warnings.ml \
  location.mli location.ml \
  longident.mli longident.ml \
  docstrings.mli docstrings.ml \
  syntaxerr.mli syntaxerr.ml \
  ast_helper.mli ast_helper.ml \
  ast_iterator.mli ast_iterator.ml \
  builtin_attributes.mli builtin_attributes.ml \
  camlinternalMenhirLib.mli camlinternalMenhirLib.ml \
  parser.mli parser.ml \
  lexer.mli lexer.ml \
  pprintast.mli pprintast.ml \
  parse.mli parse.ml \
  ocamlprof.mli ocamlprof.ml

ocamlcp_ocamloptp_SOURCES = \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  format_doc.mli format_doc.ml \
  misc.mli misc.ml \
  profile.mli profile.ml \
  warnings.mli warnings.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  terminfo.mli terminfo.ml \
  location.mli location.ml \
  ccomp.mli ccomp.ml \
  compenv.mli compenv.ml \
  main_args.mli main_args.ml \
  ocamlcp_common.mli ocamlcp_common.ml

ocamlcp_LIBRARIES =
ocamlcp_SOURCES = $(ocamlcp_ocamloptp_SOURCES) ocamlcp.mli ocamlcp.ml

ocamloptp_LIBRARIES =
ocamloptp_SOURCES = $(ocamlcp_ocamloptp_SOURCES) ocamloptp.mli ocamloptp.ml

# To help building mixed-mode libraries (OCaml + C)
ocamlmklib_LIBRARIES =
ocamlmklib_SOURCES = \
  config.ml \
  build_path_prefix_map.ml \
  format_doc.ml \
  misc.ml \
  ocamlmklib.mli ocamlmklib.ml

# To make custom toplevels

ocamlmktop_LIBRARIES =
ocamlmktop_SOURCES = \
  config.mli config.ml \
  build_path_prefix_map.mli build_path_prefix_map.ml \
  format_doc.mli format_doc.ml \
  misc.mli misc.ml \
  identifiable.mli identifiable.ml \
  numbers.mli numbers.ml \
  arg_helper.mli arg_helper.ml \
  local_store.mli local_store.ml \
  load_path.mli load_path.ml \
  clflags.mli clflags.ml \
  profile.mli profile.ml \
  ccomp.mli ccomp.ml \
  ocamlmktop.mli ocamlmktop.ml

# Reading cmt files

ocamlcmt_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
ocamlcmt_SOURCES = tools/ocamlcmt.mli tools/ocamlcmt.ml

# The bytecode disassembler

dumpobj_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
dumpobj_SOURCES = $(addprefix tools/, \
  opnames.mli opnames.ml \
  dumpobj.mli dumpobj.ml)

tools/opnames.ml: tools/opnames.ml.c runtime/caml/opcodes.h
	$(V_GEN)$(CPP) -I runtime $< > $@

clean::
	rm -f tools/opnames.ml

beforedepend:: tools/opnames.ml

# Display info on compiled files

ocamlobjinfo_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp ocamlmiddleend)
ocamlobjinfo_SOURCES = tools/objinfo.mli tools/objinfo.ml

# Scan object files for required primitives

primreq_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
primreq_SOURCES = tools/primreq.mli tools/primreq.ml

# Copy a bytecode executable, stripping debug info

stripdebug_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
stripdebug_SOURCES = tools/stripdebug.mli tools/stripdebug.ml

# Compare two bytecode executables

cmpbyt_LIBRARIES = $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp)
cmpbyt_SOURCES = tools/cmpbyt.mli tools/cmpbyt.ml

# Scan latex files, and run ocaml code examples

ocamltex_LIBRARIES = \
  $(addprefix compilerlibs/,ocamlcommon ocamlbytecomp ocamltoplevel) \
  $(addprefix otherlibs/,str/str unix/unix)
ocamltex_SOURCES = tools/ocamltex.mli tools/ocamltex.ml

# ocamltex uses str.cma and unix.cma and so must be compiled with
# $(ROOTDIR)/ocamlc rather than with $(ROOTDIR)/boot/ocamlc since the boot
# compiler does not necessarily have the correct shared library
# configuration.
# Note: the following definitions apply to all the prerequisites
# of ocamltex.
$(ocamltex): CAMLC = $(OCAMLRUN) $(ROOTDIR)/ocamlc$(EXE) $(STDLIBFLAGS)
$(ocamltex): OC_COMMON_LINKFLAGS += -linkall
$(ocamltex): VPATH += $(addprefix otherlibs/,str unix)

tools/ocamltex.cmo: OC_COMMON_COMPFLAGS += -no-alias-deps

ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "false"
# ocamltex needs a custom runtime when building statically owing to the C stubs
# in unix.cma and str.cma. This is specified explicitly to suppress the default
# linking flags (see $(MAYBE_ADD_BYTECODE_LAUNCHER_FLAGS) in Makefile.common)
tools/ocamltex$(EXE): ocamltex_BYTECODE_LINKFLAGS += -custom
endif

# we need str and unix which depend on the bytecode version of other tools
# thus we use the othertools target
## Test compilation of backend-specific parts

ARCH_SPECIFIC =\
  asmcomp/arch.mli asmcomp/arch.ml asmcomp/proc.ml asmcomp/CSE.ml \
  asmcomp/selection.ml asmcomp/scheduling.ml asmcomp/reload.ml \
  asmcomp/stackframe.ml

partialclean::
	rm -f $(ARCH_SPECIFIC)

beforedepend:: $(ARCH_SPECIFIC)

# This rule provides a quick way to check that machine-dependent
# files compiles fine for a foreign architecture (passed as ARCH=xxx).

.PHONY: check_arch
check_arch:
	@echo "========= CHECKING asmcomp/$(ARCH) =============="
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*
	@$(MAKE) compilerlibs/ocamloptcomp.cma \
	            >/dev/null
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*

.PHONY: check_all_arches
check_all_arches:
ifeq ($(ARCH64),true)
	@STATUS=0; \
	 for i in $(ARCHES); do \
	   $(MAKE) --no-print-directory check_arch ARCH=$$i || STATUS=1; \
	 done; \
	 exit $$STATUS
else
	 @echo "Architecture tests are disabled on 32-bit platforms."
endif

# The native toplevel

ocamlnat_LIBRARIES = \
  compilerlibs/ocamlcommon compilerlibs/ocamloptcomp \
  compilerlibs/ocamlbytecomp otherlibs/dynlink/dynlink \
  compilerlibs/ocamltoplevel

ocamlnat_SOURCES = $(ocaml_SOURCES)

ocamlnat_NATIVE_LINKFLAGS = -linkall -I toplevel/native

COMPILE_NATIVE_MODULE = \
  $(CAMLOPT) $(OC_COMMON_COMPFLAGS) -I $(@D) $(INCLUDES) \
  $(OC_NATIVE_COMPFLAGS)


toplevel/topdirs.cmx toplevel/toploop.cmx $(ocamlnat_CMX_FILES): \
  OC_NATIVE_COMPFLAGS += -I toplevel/native

toplevel/toploop.cmx: toplevel/native/topeval.cmx

$(ocamlnat_CMX_FILES): toplevel/native/topmain.cmx

partialclean::
	rm -f ocamlnat ocamlnat.exe

toplevel/native/topeval.cmx: otherlibs/dynlink/dynlink.cmxa

# The numeric opcodes

bytecomp/opcodes.ml: bytecomp/opcodes.ml.c runtime/caml/opcodes.h
	$(CPP) -I runtime $< > $@

bytecomp/opcodes.mli: bytecomp/opcodes.ml
	$(V_GEN)$(CAMLC) -i $< > $@

partialclean::
	rm -f bytecomp/opcodes.ml bytecomp/opcodes.mli

beforedepend:: bytecomp/opcodes.ml bytecomp/opcodes.mli

ifneq "$(wildcard .git)" ""
include Makefile.dev
endif

# Default rules

%.cmo: %.ml
	$(V_OCAMLC)$(CAMLC) $(OC_COMMON_COMPFLAGS) -I $(@D) $(INCLUDES) -c $<

%.cmi: %.mli
	$(V_OCAMLC)$(CAMLC) $(OC_COMMON_COMPFLAGS) -I $(@D) $(INCLUDES) -c $<

%.cmx: %.ml
	$(V_OCAMLOPT)$(COMPILE_NATIVE_MODULE) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp middle_end file_formats \
           lambda middle_end/closure middle_end/flambda \
           middle_end/flambda/base_types \
           driver toplevel toplevel/byte toplevel/native tools debugger; do \
	  rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.s $$d/*.asm \
	    $$d/*.o $$d/*.obj $$d/*.so $$d/*.dll; \
	done

%.depend: beforedepend
	$(V_OCAMLDEP)$(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I $* $(INCLUDES) \
	  $(OCAMLDEPFLAGS) $*/*.mli $*/*.ml > $@

asmcomp.depend:: beforedepend $(cvt_emit)
	$(V_OCAMLDEP)$(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I asmcomp $(INCLUDES) \
	  $(OCAMLDEPFLAGS) $(filter-out $(ARCH_SPECIFIC) asmcomp/emit.ml, \
	                                $(wildcard asmcomp/*.mli asmcomp/*.ml)) > $@

partialclean::
	rm -f $(addsuffix .depend, $(ARCH_SPECIFIC) asmcomp/emit.ml)

# asmcomp.depend contains the dependencies for all the backends, with ifeq used
# to select the correct one depending on the ARCH variable. In order to
# generate this file, we must temporarily replace the $(ARCH_SPECIFIC) files
# with the ones for each architecture. At the end of this process, the files for
# the active architecture (i.e. $(ARCH)) must be restored, but if we simply
# re-link the files we will trigger a complete rebuild of the native compiler
# and .opt binaries. The recipe for asmcomp.depend therefore begins by renaming
# the existing files, then it generates asmcomp.depend and then we rename the
# files back. This means their timestamps are unaltered, and the next invocation
# of make therefore correctly doesn't rebuild anything.

define MV_FILE
asmcomp.depend::
	@mv $(1) $(2)

endef

$(foreach file, asmcomp/emit.ml $(ARCH_SPECIFIC),\
  $(eval $(call MV_FILE,$(file),$(file).depend)))

define ADD_ARCH_SPECIFIC_DEPS
asmcomp.depend::
	@echo 'ifeq "$$$$(ARCH)" "$(1)"' > asmcomp/$(1).depend
	@$$(MAKE) ARCH=$(1) asmcomp/emit.ml $$(ARCH_SPECIFIC)
	@$$(OCAMLDEP) $$(OC_OCAMLDEPFLAGS) -I asmcomp $$(INCLUDES) \
	  $$(OCAMLDEPFLAGS) asmcomp/emit.ml $$(ARCH_SPECIFIC) >> asmcomp/$(1).depend
	@echo 'endif # ifeq "$$$$(ARCH)" "$(1)"' >> asmcomp/$(1).depend
	@rm -f asmcomp/emit.ml $$(ARCH_SPECIFIC)

endef

$(foreach arch, $(ARCHES),\
  $(eval $(call ADD_ARCH_SPECIFIC_DEPS,$(arch))))

asmcomp.depend::
	@cat $(addprefix asmcomp/, $(addsuffix .depend, $(ARCHES))) >> $@
	@rm -f $(addprefix asmcomp/, $(addsuffix .depend, $(ARCHES)))

$(foreach file, asmcomp/emit.ml $(ARCH_SPECIFIC),\
  $(eval $(call MV_FILE,$(file).depend,$(file))))

DEP_DIRS = \
  utils parsing typing bytecomp asmcomp middle_end lambda file_formats \
  middle_end/closure middle_end/flambda middle_end/flambda/base_types driver \
  toplevel toplevel/byte toplevel/native lex tools debugger ocamldoc ocamltest \
  testsuite/lib testsuite/tools otherlibs/dynlink

DEP_FILES = $(addsuffix .depend, $(DEP_DIRS))

.INTERMEDIATE: $(DEP_FILES)

.PHONY: depend
depend: $(DEP_FILES) | beforedepend
	$(V_GEN)cat $^ > .$@
	@rm -f $(DYNLINK_DEPEND_DUMMY_FILES)
>>>>>>> ocaml/ocaml#14498

.PHONY: distclean
distclean: clean
<<<<<<< HEAD
	$(if $(filter 1,$(V)),,@)set -eu; \
	  dirs="$(DISTCLEAN_DIRS)"; \
	  if [ -z "$$dirs" ]; then echo "Refusing to distclean empty directory list" >&2; exit 1; fi; \
	  for dir in $$dirs; do \
	    case "$$dir" in ""|"/"|".") echo "Refusing to distclean $$dir" >&2; exit 1;; esac; \
	  done; \
	  rm -rf -- $$dirs; \
	  rm -f -- $(DISTCLEAN_FILES)
||||||| upstream
ifneq "$(FLEXDLL_SUBMODULE_PRESENT)" ""
	$(MAKE) -C flexdll distclean MSVC_DETECT=0
endif
	$(MAKE) -C manual distclean
	rm -f ocamldoc/META
	rm -f $(addprefix ocamltest/,ocamltest_config.ml ocamltest_unix.ml)
	rm -f otherlibs/dynlink/META otherlibs/dynlink/dynlink_config.ml \
	  otherlibs/dynlink/dynlink_cmo_format.mli \
	  otherlibs/dynlink/dynlink_cmxs_format.mli \
	  otherlibs/dynlink/dynlink_platform_intf.mli
	$(MAKE) -C otherlibs distclean
	rm -f $(runtime_CONFIGURED_HEADERS) runtime/ld.conf
	$(MAKE) -C stdlib distclean
	$(MAKE) -C testsuite distclean
	rm -f tools/eventlog_metadata tools/*.bak
	rm -f utils/config.common.ml utils/config.generated.ml
	rm -f compilerlibs/META
	rm -f boot/ocamlrun boot/ocamlrun.exe boot/$(HEADER_NAME) \
	      boot/flexdll_*.o boot/flexdll_*.obj \
	      boot/*.cm* boot/libcamlrun.a boot/libcamlrun.lib boot/ocamlc.opt
	rm -f Makefile.config Makefile.build_config
	rm -rf autom4te.cache flexdll-sources \
         $(BYTE_BUILD_TREE) $(OPT_BUILD_TREE)
	rm -f config.log config.status libtool
=======
ifneq "$(FLEXDLL_SUBMODULE_PRESENT)" ""
	$(MAKE) -C flexdll distclean MSVC_DETECT=0
endif
	$(MAKE) -C manual distclean
	rm -f ocamldoc/META
	rm -f $(addprefix ocamltest/,ocamltest_config.ml ocamltest_unix.ml)
	rm -f otherlibs/dynlink/META otherlibs/dynlink/dynlink_config.ml \
	  otherlibs/dynlink/dynlink_cmo_format.mli \
	  otherlibs/dynlink/dynlink_cmxs_format.mli \
	  otherlibs/dynlink/dynlink_platform_intf.mli
	$(MAKE) -C otherlibs distclean
	rm -f $(runtime_CONFIGURED_HEADERS) runtime/ld.conf
	rm -rf runtime/api-testing
	$(MAKE) -C stdlib distclean
	$(MAKE) -C testsuite distclean
	rm -f tools/eventlog_metadata tools/*.bak
	rm -f utils/config.common.ml utils/config.generated.ml
	rm -f compilerlibs/META
	rm -f boot/ocamlrun boot/ocamlrun.exe boot/$(HEADER_NAME) \
	      boot/flexdll_*.o boot/flexdll_*.obj \
	      boot/*.cm* boot/libcamlrun.a boot/libcamlrun.lib boot/ocamlc.opt
	rm -f Makefile.config Makefile.build_config
	rm -rf autom4te.cache flexdll-sources \
         $(BYTE_BUILD_TREE) $(OPT_BUILD_TREE)
	rm -f config.log config.status libtool
>>>>>>> ocaml/ocaml#14498

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
# 	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_main) @chamelon/all

.PHONY: boot-minimizer
boot-minimizer:
	cp chamelon/compat/dune.ox chamelon/compat/dune
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) @chamelon/all

.PHONY: minimizer
minimizer: runtime-stdlib
	cp chamelon/compat/dune.ox chamelon/compat/dune
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

.PHONY: regen-flambda2-parser-messages
regen-flambda2-parser-messages: $(dune_config_targets)
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) @middle_end/flambda2/parser/regen-messages --auto-promote || true

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
