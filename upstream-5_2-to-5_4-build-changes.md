# Upstream OCaml 5.2 to 5.4 Build System Changes

This document summarizes changes in the upstream Makefile between OCaml 5.2 and 5.4.
OxCaml uses dune instead of the traditional Makefile, so these changes may need to be
reflected in dune files or other OxCaml-specific build configuration.

## New Source Files

### utils/
- `format_doc.mli`, `format_doc.ml` - Format documentation utilities
- `linkdeps.mli`, `linkdeps.ml` - Link dependencies tracking
- `compression.mli`, `compression.ml` - Compression support (zstd)

### parsing/
- `asttypes.ml` - Now has implementation file (previously just `.mli`)

### typing/
- `data_types.mli`, `data_types.ml` - Data types utilities
- `rawprinttyp.mli`, `rawprinttyp.ml` - Raw type printing
- `gprinttyp.mli`, `gprinttyp.ml` - Generic type printing
- `out_type.mli`, `out_type.ml` - Output type utilities
- `errortrace_report.mli`, `errortrace_report.ml` - Error trace reporting
- `shape_reduce.mli`, `shape_reduce.ml` - Shape reduction

### lambda/
- `value_rec_compiler.mli`, `value_rec_compiler.ml` - Value recursion compiler

**Status**: All these files exist in the source tree, and may need adding to dune files.

## Compression Support (zstd)

New linking flags for compressed marshaling:
```makefile
COMPRESSED_MARSHALING_FLAGS=-cclib -lcomprmarsh \
           $(patsubst %, -ccopt %, $(filter-out -l%,$(ZSTD_LIBS))) \
           $(patsubst %, -cclib %, $(filter -l%,$(ZSTD_LIBS)))

compilerlibs/ocamlcommon.cmxa: \
  OC_NATIVE_LINKFLAGS += $(COMPRESSED_MARSHALING_FLAGS)

compilerlibs/ocamlcommon.cmxa: stdlib/libcomprmarsh.$(A)
```

**Action needed?** Check if anything is needed for OxCaml here.

## New Tools

- `tools/sync_dynlink.opt` - Added to `OCAML_NATIVE_PROGRAMS`

**Action needed?** Check if this tool should be built in OxCaml.

## Conditional ocamlobjinfo

`ocamlobjinfo` is now conditionally built based on `build_ocamlobjinfo`:
```makefile
TOOLS_TO_INSTALL_NAT = ocamldep
ifeq "$(build_ocamlobjinfo)" "true"
  TOOLS_TO_INSTALL_NAT += ocamlobjinfo
endif
```

## LIBFILES Variable

Changed from hardcoded `camlheader` to `$(HEADER_NAME)`:
```makefile
# Before (5.2)
LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

# After (5.4)
LIBFILES=stdlib.cma std_exit.cmo *.cmi $(HEADER_NAME)
```

**Status**: OxCaml uses dune for installation, so this is likely not relevant.

## FlexDLL Changes (Windows)

- FlexDLL objects now copied to `$(ROOTDIR)` in addition to `$(BYTE_BINDIR)`
- Additional clean targets for flexdll submodule/sources
- Runtime dependency list updated to include shared libraries
- `FLEXDLL_MANIFEST` pattern changed: `default_$(ARCH).manifest` -> `default$(filter-out _i386,_$(ARCH)).manifest`

**Status**: Windows-specific, not relevant yet for OxCaml.

## Runtime Changes

### New C Source Files
- `runtime/fail.c` - Added to common C sources
- `runtime/zstd.c` - Added to bytecode-only sources (compression support)

### Windows pthreads Support
New `winpthreads_OBJECTS` for Windows builds:
```makefile
ifneq "$(WINPTHREADS_SOURCE_DIR)" ""
winpthreads_SOURCES = cond.c misc.c mutex.c rwlock.c sched.c spinlock.c thread.c
winpthreads_OBJECTS = $(addprefix runtime/winpthreads/, $(winpthreads_SOURCES:.c=.$(O)))
endif
```

### New Runtime Library
- `runtime/libcomprmarsh.$(A)` - Compression marshaling library (uses zstd)
- Added to `runtime_NATIVE_STATIC_LIBRARIES`

### TSan Sources Reorganization
- `TSAN_NATIVE_RUNTIME_C_SOURCES` moved from common to native-only section

### SAK Build Simplification
SAK (Swiss Army Knife) build simplified to single-step:
```makefile
# Before (5.2)
$(SAK): runtime/sak.$(O)
	$(V_MKEXE)$(call SAK_LINK,$@,$^)
runtime/sak.$(O): runtime/sak.c ...
	$(V_CC)$(SAK_CC) -c $(SAK_CFLAGS) $(OUTPUTOBJ)$@ $<

# After (5.4)
$(SAK): runtime/sak.c runtime/caml/misc.h runtime/caml/config.h
	$(V_MKEXE)$(call SAK_BUILD,$@,$<)
```

### Build Flags Changes
Changed from additive (`+=`) to explicit (`=`) assignment for runtime object flags:
```makefile
# Before (5.2)
runtime/%.$(O): OC_CPPFLAGS += $(runtime_CPPFLAGS)

# After (5.4)
runtime/%.b.$(O): OC_CFLAGS = $(OC_BYTECODE_CFLAGS)
runtime/%.b.$(O): OC_CPPFLAGS = $(OC_BYTECODE_CPPFLAGS) $(ocamlrun_CPPFLAGS)
```

### Other Runtime Changes
- `C_LITERAL` now uses `$(ENCODE_C_LITERAL)` variable instead of hardcoded `encode-C-literal`
- `build_config.h` now uses `$(TARGET_LIBDIR)` instead of `$(LIBDIR)`
- `names_of_*` arrays now `static char const * const` (was `static char *`)
- Comment updated: "labels as values extension" instead of "GCC 2.0 or later"

**Status**: OxCaml's runtime is built via dune. These changes affect the traditional Makefile only.

## Dynlink Library Changes

Dynlink build moved from sub-makefile to root Makefile:
```makefile
dynlink_SOURCES = $(addprefix otherlibs/dynlink/,\
  dynlink_config.mli dynlink_config.ml \
  dynlink_types.mli dynlink_types.ml \
  dynlink_platform_intf.mli dynlink_platform_intf.ml \
  dynlink_common.mli dynlink_common.ml \
  byte/dynlink_symtable.mli byte/dynlink_symtable.ml \
  byte/dynlink.mli byte/dynlink.ml \
  native/dynlink.mli native/dynlink.ml)
```

New targets:
- `dynlink-all` - Build bytecode dynlink
- `dynlink-allopt` - Build native dynlink

New tool for syncing dynlink sources:
```makefile
sync_dynlink_SOURCES = tools/sync_dynlink.mli tools/sync_dynlink.ml
```

## Tool Source Changes

New files added to tool source lists:

### ocamlprof, ocamlcp/ocamloptp, ocamlmklib, ocamlmktop
- `format_doc.mli`, `format_doc.ml` - Added to all these tools

### ocamltest
New debugger-related sources:
- `debugger_flags.mli`, `debugger_flags.ml`
- `debugger_variables.mli`, `debugger_variables.ml`
- `debugger_actions.mli`, `debugger_actions.ml`

### ocamldoc
- Added `CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)` target-specific variable
- Changed dependency: `ocamldoc.opt$(EXE)` now depends on `ocamlopt` instead of `ocamlc.opt`

### ocamltest
- Added `CAMLOPT = $(BEST_OCAMLOPT) $(STDLIBFLAGS)` target-specific variable
- Added `-g` to bytecode link flags
- Changed dependency: `ocamltest.opt$(EXE)` now depends on `ocamlopt` instead of `ocamlc.opt`

**Status**: Tools are built via dune in OxCaml.

## Dependency Generation Restructuring

Major changes to how `.depend` files are generated:

### Per-Directory Depend Files
```makefile
%.depend: beforedepend
	$(V_OCAMLDEP)$(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I $* $(INCLUDES) \
	  $(OCAMLDEPFLAGS) $*/*.mli $*/*.ml > $@
```

### Architecture-Specific Dependencies
New handling for architecture-specific files with `ifeq "$(ARCH)"` conditionals:
```makefile
$(foreach arch, $(ARCHES),\
  $(eval $(call ADD_ARCH_SPECIFIC_DEPS,$(arch))))
```

### DEP_DIRS Variable
```makefile
DEP_DIRS = \
  utils parsing typing bytecomp asmcomp middle_end lambda file_formats \
  middle_end/closure middle_end/flambda middle_end/flambda/base_types driver \
  toplevel toplevel/byte toplevel/native lex tools debugger ocamldoc ocamltest \
  testsuite/lib testsuite/tools otherlibs/dynlink
```

**Status**: OxCaml uses dune for dependency tracking.

## Debugger Changes

- `ocamldebug_BYTECODE_LINKFLAGS = -linkall` (was target-specific rule)

## Installation Changes

### New Dynlink Installation Directory
Dynlink is now installed to a separate subdirectory:
```makefile
INSTALL_LIBDIR_DYNLINK = $(INSTALL_LIBDIR)/dynlink
```

Both bytecode (`dynlink.cmi`, `dynlink.cma`, `META`) and native (`dynlink.cmxa`, `dynlink.$(A)`, `.cmx` files) are installed there.

### Cleanup of Old Dynlink Installation
When installing, old dynlink files from previous OCaml versions are removed:
```makefile
rm -f "$(INSTALL_LIBDIR)"/dynlink.cm* "$(INSTALL_LIBDIR)/dynlink.mli" \
      "$(INSTALL_LIBDIR)/dynlink.$(A)" \
      $(addprefix "$(INSTALL_LIBDIR)/", $(notdir $(dynlink_CMX_FILES)))
```

### Variable Rename
- `OTHERLIBRARIES` → `OTHERLIBS` in install loop

## Cross-Compiler Support

New cross-compilation recipes:
```makefile
# Include the cross-compiler recipes only when relevant
ifneq "$(HOST)" "$(TARGET)"
include Makefile.cross
endif
```

## Misc Changes

- `tools/stripdebug` now consistently uses `$(EXE)` extension
- VPATH now includes `runtime` directory
- Various partialclean targets updated
- Removed `ocamlc.opt$(EXE): OC_NATIVE_LINKFLAGS += $(addprefix -cclib ,$(BYTECCLIBS))`
- `subdirs` variable simplified to explicit list instead of using `$(ALL_OTHERLIBS)`
- Added `yacc/wstr.o yacc/wstr.obj` to clean target

## GitHub Actions CI Changes

OxCaml has its own CI configuration, but upstream made some changes that may be relevant:

### Actions Version Updates
- `actions/checkout@v3` → `v5`
- `actions/upload-artifact@v3` → `v4`
- `actions/download-artifact@v3` → `v4`

**Status**: OxCaml CI should update these versions when convenient.

### New `config` Job
Upstream added a `config` job that:
- Computes the test matrix dynamically using `actions/github-script@v7`
- Supports a `CI: Skip testsuite` label on PRs to skip tests
- Supports a `CI: Full matrix` label to add extra jobs (static, minimal builds)

**Status**: Could be useful for OxCaml CI if similar flexibility is desired.

### New Test Dependencies
Added to ubuntu dependencies: `texlive-luatex`, `gdb`, `lldb`

**Status**: OxCaml may want `gdb`/`lldb` for debugging tests.

### Configure Flag
Added `--enable-codegen-invariants` to the default configure arguments.

**Status**: OxCaml already has `--enable-cmm-invariants`; check if codegen-invariants is different/additional.

### macOS Debugger Setup
New macOS setup for lldb support in CI:
```bash
# Allows starting up lldb from a remote terminal
sudo DevToolsSecurity --enable
spctl developer-mode enable-terminal
sudo xcode-select -s /Applications/Xcode.app/Contents/Developer
lldb --version
```

**Status**: Useful for running OxCaml DWARF tests on macOS in the future.
