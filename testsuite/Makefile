#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

.NOTPARALLEL:

BASEDIR := $(shell pwd)
ifneq "$(words |$(BASEDIR)|)" "1"
$(error The Testsuite does not handle spaces\
   in the ocaml working directory path: $(BASEDIR))
endif

ROOTDIR = ..
include $(ROOTDIR)/Makefile.common

ifeq "$(UNIX_OR_WIN32)" "win32"
  CYGPATH=cygpath -m
  # Ensure that the test runners definitely use Cygwin's sort and not the
  # Windows sort command
  SORT=/usr/bin/sort
else
  CYGPATH=echo
  SORT=sort
endif

BASEDIR_HOST := $(shell $(CYGPATH) "$(BASEDIR)")
ROOTDIR_HOST := $(BASEDIR_HOST)/$(ROOTDIR)

# TEST_ID should be set by the user (to a label made of letters, digits,
# '-' and '_') to run several passes at once in one tree, e.g. with
# different CODEGEN_VARIANT values: the build directories of the tests, the
# log and the failure stamp of a pass are then named after its label
# (_ocamltest.<label>, _log.<label>, failure.stamp.<label>), and
# 'promote-failed' and 'report' with the same TEST_ID read that log.
TEST_ID ?=
test_id_suffix = $(if $(TEST_ID),.$(TEST_ID))

ocamltestdir_name = _ocamltest$(test_id_suffix)

OCAMLTESTDIR = $(BASEDIR_HOST)/$(DIR)/$(ocamltestdir_name)

# ocamltest itself builds the tests in _ocamltest when it is not told where.
OCAMLTESTENV ?= \
  $(if $(TEST_ID),OCAMLTESTDIR=$(BASEDIR_HOST)/$(ocamltestdir_name))

failstamp := failure.stamp$(test_id_suffix)

# The shell reports the status of the last command of a pipeline only, so a
# command that fails on the left of a '|' (ocamltest listing the tests,
# say, or GNU parallel) would go unnoticed, and the run would report the
# tests that it did run as the whole result.
# $(call or_stamp,<what>,<command>) runs <command> and records its failure
# in $(failstamp), as <what>, for check-failstamp to report.
or_stamp = { $(2) || echo "$(1) failed" >> $(failstamp); }

TESTLOG ?= _log$(test_id_suffix)

# The log of the 'one' target, which leaves the log of the last whole run
# alone. The 'list' target is a whole run: it logs to $(TESTLOG), where
# 'promote-failed' and 'report' look.
ONELOG ?= $(TESTLOG).one

# 'list' gives ONELOG on the command line of the 'one' that it runs, which
# would hand it on to the tests through their environment. A test may start
# this makefile itself (tool-ocamltest/test_lists.ml does), and its 'one'
# would then write into the log of the run that the test is part of.
unexport ONELOG

ocamltest_directory := ../ocamltest

ocamltest_program := $(or \
  $(wildcard $(ocamltest_directory)/ocamltest.opt$(EXE)),\
  $(wildcard $(ocamltest_directory)/ocamltest$(EXE)))

ifeq "$(UNIX_OR_WIN32)" "unix"
  ifeq "$(SYSTEM)" "cygwin"
    find := /usr/bin/find
  else # Non-cygwin Unix
    find := find
  endif
  # $(IFS_LINE) for Unix simply clears IFS, so entire lines are read
  IFS_LINE = IFS=''
else # Windows
  find := /usr/bin/find
  # On Windows, ocamltest will produce Windows line-endings (\r\n) and the final
  # \r is kept by the shell. This can either be stripped with tr -d '\r' but we
  # can avoid the additional process by instead setting IFS in the while loop to
  # be the CR character (i.e. treat \r as a field delimiter).
  # The dance with $(CR_CHAR) is because POSIX doesn't provide a way to write \r
  # in a string.
  export CR_CHAR := $(shell printf "\r")
  IFS_LINE = IFS="$$CR_CHAR"
endif

# The options and the command-line variables of this make are not for the
# makes that tests start. Such a make would otherwise give its recipes the
# value that a variable has on this command line (OCAMLPARAM, say) in place
# of the one that ocamltest gave the test. Make also puts its command-line
# variables in the environment, where a testsuite makefile that a test
# starts would take the variant of this run for its own: CODEGEN_VARIANT
# and VARIANT_ONLY are for the arguments of ocamltest, not for the tests.
ifeq "$(ocamltest_program)" ""
  ocamltest = $(error ocamltest not found in $(ocamltest_directory))
else
  ocamltest := env -u MAKEFLAGS -u MFLAGS -u MAKELEVEL \
    -u CODEGEN_VARIANT -u VARIANT_ONLY \
    SORT=$(SORT) MAKE=$(MAKE) $(ocamltest_program)
endif

# PROMOTE is only meant to be used internally in recursive calls;
# users should call the 'promote' target explicitly.
PROMOTE =
ifeq "$(PROMOTE)" ""
  OCAMLTEST_PROMOTE_FLAG =
else
  OCAMLTEST_PROMOTE_FLAG = -promote
endif

# KEEP_TEST_DIR_ON_SUCCESS should be set by the user (to a non-empty value)
# if they want to pass the -keep-test-dir-on-success option to ocamltest,
# to preserve test data of successful tests.

# KEEP is provided as a rather easier to remmber alias of
# KEEP_TEST_DIR_ON_SUCCESS, but to prevent the risk of naming conflict it's only
# recognised when used as make -C testsuite KEEP=1 ...
ifeq "$(origin KEEP)" "command line"
KEEP_TEST_DIR_ON_SUCCESS ?= $(KEEP)
else
KEEP_TEST_DIR_ON_SUCCESS ?=
endif

ifeq "$(KEEP_TEST_DIR_ON_SUCCESS)" ""
  OCAMLTEST_KEEP_TEST_DIR_ON_SUCCESS_FLAG =
else
  OCAMLTEST_KEEP_TEST_DIR_ON_SUCCESS_FLAG = -keep-test-dir-on-success
endif

TIMEOUT ?= 600 # 10 minutes

# SHOW_TIMINGS should be set by the user (to a non-empty value) if they want
# the timings for each test file to be included in the log
SHOW_TIMINGS ?=
OCAMLTEST_SHOW_TIMINGS_FLAG =
ifneq "$(SHOW_TIMINGS)" ""
ifeq "$(lib_unix)" "true"
  OCAMLTEST_SHOW_TIMINGS_FLAG = -show-timings
endif
endif

# RUNPARAM should be set by the user to pass extra OCAMLRUNPARAM
# components to the test programs that ocamltest runs (the compilers
# and other tools are unaffected), e.g.
#   make one TEST=tests/foo/bar.ml RUNPARAM=v=0x400
RUNPARAM ?=
ifeq "$(RUNPARAM)" ""
  OCAMLTEST_RUNPARAM_FLAG =
else
  OCAMLTEST_RUNPARAM_FLAG = -run-ocamlrunparam '$(RUNPARAM)'
endif

# CODEGEN_VARIANT should be set by the user to run the tests for another
# codegen variant of the compiler under test (see ocamlopt -config-var
# codegen_variants), e.g.
#   make one DIR=tests/effects CODEGEN_VARIANT=fp
# The tests then run for that variant instead of base: the native programs
# that ocamltest compiles and links are built for it, and native expect,
# toplevel and asmgen tests run in the builds of their host programs for it
# (see ocamltest/OCAMLTEST.org). A test that selects a variant itself keeps
# its selection. Naming base is the same as not setting the variable.
CODEGEN_VARIANT ?=
ifeq "$(CODEGEN_VARIANT)" ""
  OCAMLTEST_CODEGEN_VARIANT_FLAG =
else
  OCAMLTEST_CODEGEN_VARIANT_FLAG = -codegen-variant '$(CODEGEN_VARIANT)'
endif

# VARIANT_ONLY should be set by the user (to a non-empty value), together
# with CODEGEN_VARIANT, for a quick pass that leaves out the test files in
# which nothing depends on the variant (those that only run bytecode, say):
# such a pass supplements a pass without CODEGEN_VARIANT and does not
# replace it.
VARIANT_ONLY ?=
ifeq "$(VARIANT_ONLY)" ""
  OCAMLTEST_CODEGEN_VARIANT_ONLY_FLAG =
else
  OCAMLTEST_CODEGEN_VARIANT_ONLY_FLAG = -codegen-variant-only
endif

OCAMLTESTFLAGS = \
  -exit-0-on-failure \
  -timeout $(TIMEOUT) \
  $(OCAMLTEST_PROMOTE_FLAG) \
  $(OCAMLTEST_KEEP_TEST_DIR_ON_SUCCESS_FLAG) \
  $(OCAMLTEST_SHOW_TIMINGS_FLAG) \
  $(OCAMLTEST_RUNPARAM_FLAG) \
  $(OCAMLTEST_CODEGEN_VARIANT_FLAG) \
  $(OCAMLTEST_CODEGEN_VARIANT_ONLY_FLAG)

# Make sure USE_RUNTIME is defined
USE_RUNTIME ?=

ifneq ($(USE_RUNTIME),)
# Check USE_RUNTIME value
ifeq ($(findstring $(USE_RUNTIME),d i),)
$(error If set, USE_RUNTIME must be equal to "d" (debug runtime) \
        or "i" (instrumented runtime))
endif
# When using the debug or instrumented runtime,
# set the runtime's verbosity to 0 by default
export OCAMLRUNPARAM?=v=0
endif

# Remove these from the environment to prevent text unexpectedly wrapping with
# -d options.
unexport ROWS
unexport COLUMNS

.PHONY: default
default:
	@echo "Available targets:"
	@echo "  all                        launch all tests"
	@echo "  all-foo                    launch all tests beginning with foo"
	@echo "  parallel                   launch all tests using GNU parallel"
	@echo "  parallel-foo               launch all tests beginning with foo using \
	GNU parallel"
	@echo "  one (TEST|DIR|LIST)=x      launch the specified tests ..."
	@echo "  promote (TEST|DIR|LIST)=x  promote the output for the specified \
	tests ..."
	@echo "    ... TEST=f                 ... the single test in the file f"
	@echo "    ... DIR=d                  ... the tests located in the directory d"
	@echo "    ... LIST=f                 ... the tests listed in the file f (one \
	per line)"
	@echo "  promote-failed             promote all the tests that failed in the previous run"
	@echo "  clean                      delete generated files"
	@echo "  report                     print the report for the last execution"
	@echo
	@echo "By default, artefacts from tests which pass are not kept, but this can"
	@echo "be changed by adding KEEP=1 to the make command line or by setting the"
	@echo "KEEP_TEST_DIR_ON_SUCCESS environment variable to a non-empty value."
	@echo
	@echo "Set the environment variable USE_RUNTIME to \"d\" or \"i\" to run"
	@echo "the tests with the debug or the instrumented runtime."
	@echo
	@echo "Set CODEGEN_VARIANT to the name of a codegen variant of the compiler"
	@echo "(e.g. fp) to run the tests for that variant instead of base. Add"
	@echo "VARIANT_ONLY=1 to skip the tests in which nothing depends on it."
	@echo
	@echo "Set TEST_ID to a label to run several passes at once in one tree:"
	@echo "each pass then uses _ocamltest.<label> directories and _log.<label>."

.PHONY: all
all:
	@$(MAKE) --no-print-directory new-without-report
	@$(MAKE) --no-print-directory report

.PHONY: new-without-report
new-without-report:
	@rm -f $(failstamp)
	@($(call or_stamp,listing the directories of tests,\
	    $(ocamltest) -find-test-dirs tests) \
	 | while $(IFS_LINE) read -r dir; do \
	  echo Running tests from \'$$dir\' ... ; \
	  $(MAKE) exec-ocamltest DIR="$$dir" \
	    OCAMLTESTENV="$(strip $(OCAMLTESTENV))"; \
	done || echo outer loop >> $(failstamp)) 2>&1 | tee $(TESTLOG)
	@$(MAKE) check-failstamp

.PHONY: check-failstamp
check-failstamp:
	@if [ -f $(failstamp) ]; then \
	  echo 'Unexpected error in the test infrastructure:'; \
	  cat $(failstamp); \
	  rm $(failstamp); \
	  exit 1; \
	fi

.PHONY: all-%
all-%:
	@rm -f $(failstamp)
	@for dir in tests/$**; do \
	  $(MAKE) --no-print-directory exec-one DIR=$$dir; \
	done 2>&1 | tee $(TESTLOG)
	@$(MAKE) check-failstamp
	@$(MAKE) report

# The targets below use GNU parallel to parallelize tests
# 'make all' and 'make parallel' should be equivalent
#
# parallel uses specific logic to make sure the output of the commands
# run in parallel are not mangled. By default, it will reproduce
# the output of each completed command atomically, in order of completion.
#
# With the --keep-order option, we ask it to save the completed output
# and replay them in invocation order instead. In theory this costs
# a tiny bit of performance, but I could not measure any difference.
# In theory again, the reporting logic works fine with test outputs
# coming in in arbitrary order (so we should not need --keep-order),
# but keeping the output deterministic is guaranteed to make
# someone's life easier at least once in the future.
#
# Finally, note that the command we run has a 2>&1 redirection, as
# in the other make targets. If we removed the quoting around
# "$(MAKE) ... 2>&1", the rediction would apply to the complete output
# of parallel, and have a slightly different behavior: by default parallel
# cleanly separates the stdout and stderr output of each completed command,
# printing stderr first then stdout second (for each command).
# I chose to keep the previous behavior exactly unchanged,
# but the demangling separation is arguably nicer behavior that we might
# want to implement at the exec-one level to also have it in the 'all' target.

# If make has been invoked with "-j n", pass this on to GNU parallel. parallel
# does not support -j without an argument, hence the double-filter. Note that
# GNU make normalises -j in $(MAKEFLAGS) so it will either be -j alone or -jn
# (i.e. with no space).
# TEST_JOBS, when set, takes precedence: it is the number of tests to run at
# once, for callers whose own -j means something else (several passes at
# once, each of them parallel).
TEST_JOBS ?=
J_ARGUMENT = $(if $(TEST_JOBS),-j$(TEST_JOBS),\
  $(filter-out -j,$(filter -j%,$(MAKEFLAGS))))

.PHONY: parallel-%
parallel-%:
	@echo | parallel >/dev/null 2>/dev/null \
	 || (echo "Unable to run the GNU parallel tool;";\
	     echo "You should install it before using the parallel* targets.";\
	     exit 1)
	@echo | parallel --gnu --no-notice >/dev/null 2>/dev/null \
	 || (echo "Your 'parallel' tool seems incompatible with GNU parallel.";\
	     echo "This target requires GNU parallel.";\
	     exit 1)
	@rm -f $(failstamp)
	@$(call or_stamp,listing the tests,$(ocamltest) \
	    $(or $(addprefix -find-tests ,$(sort $(wildcard tests/$**))),\
	      $(error No test directory matches 'tests/$**'))) \
	 | $(call or_stamp,GNU parallel,\
	     parallel --gnu --no-notice --keep-order $(J_ARGUMENT) \
	     "$(MAKE) --no-print-directory exec-test-file TEST={} 2>&1") \
	 | tee $(TESTLOG)
	@$(MAKE) check-failstamp
	@$(MAKE) report

.PHONY: parallel
parallel: parallel-*

# A list of tests is a file that names directories of tests, relative to
# this directory, in the form tests/<directory>, one per line.
# $(call list_dirs,<file>) prints the lines without what does not count:
# empty lines, the blanks around a name, a ./ before it, and the slashes
# after it that the completion of a shell adds.
list_dirs = sed -e 's/^[[:blank:]]*//' -e 's/[[:blank:]]*$$//' -e '/^$$/d' \
  -e 's|^\(\./\)*\(.\)|\2|' -e 's|\(.\)/*$$|\1|' '$(1)'

# $(call check_list,<file>) fails, before any test runs, unless the list
# names at least one directory and every line is one name of that form
# that is a directory, has tests in it or below it, and is neither
# repeated nor inside another directory of the list. Otherwise ocamltest
# would stop at a path that does not exist, without running the
# directories after it in the list; it would find nothing to run where
# there is no test, and the run would succeed; and it would run a test
# that two names lead to twice, possibly at once in one build directory.
# A line with blanks inside is not a name: the testsuite does not handle
# them, and two names on a line would otherwise be taken apart and run.
# An ocamltest that fails where it is asked for the tests of the list has
# said why: that is not a list without tests.
check_list_lines = awk -v list='$(1)' ' \
  /[ \t]/ { \
    printf("\047%s\047, in the list \047%s\047, is not of the form " \
           "tests/<directory>.\n", $$0, list); \
    status = 1; \
  } \
  END { exit status }'
check_list_overlaps = awk -v list='$(1)' ' \
  { count[$$0]++ } \
  END { \
    for (dir in count) { \
      if (count[dir] > 1) { \
        printf("%s is in the list %s %d times.\n", dir, list, count[dir]); \
        status = 1; \
      } \
      parent = dir; \
      while (sub(/\/[^\/]*$$/, "", parent)) \
        if (parent in count) { \
          printf("%s, in the list %s, is inside %s, which is in the " \
                 "list too.\n", dir, list, parent); \
          status = 1; \
        } \
    } \
    exit status; \
  }'
check_list_tests = awk -v list='$(1)' -v dirs="$$*" ' \
  BEGIN { count = split(dirs, dir, " ") } \
  { \
    for (i = 1; i <= count; i++) \
      if ($$0 == dir[i] || index($$0, dir[i] "/") == 1) has_tests[i] = 1; \
  } \
  END { \
    for (i = 1; i <= count; i++) \
      if (!(i in has_tests)) { \
        printf("%s, in the list %s, has no test.\n", dir[i], list); \
        status = 1; \
      } \
    exit status; \
  }'
check_list = \
  if [ ! -f '$(1)' ]; then \
    echo "File '$(1)' does not exist." >&2; exit 1; \
  fi; \
  $(call list_dirs,$(1)) | $(call check_list_lines,$(1)) >&2 || exit 1; \
  set -f; set -- `$(call list_dirs,$(1))`; set +f; \
  if [ -z "$$*" ]; then \
    echo "The list '$(1)' names no directory." >&2; exit 1; \
  fi; \
  status=0; \
  for dir do \
    case "$$dir" in \
      */|*//*|*/./*|*/.|*/../*|*/..) well_formed=false;; \
      tests/?*) well_formed=true;; \
      *) well_formed=false;; \
    esac; \
    if ! $$well_formed; then \
      echo "'$$dir', in the list '$(1)', is not of the form" \
        "tests/<directory>." >&2; \
      status=1; \
    elif [ ! -d "$$dir" ]; then \
      echo "'$$dir', in the list '$(1)', is not a directory" \
        "of `pwd`." >&2; \
      status=1; \
    fi; \
  done; \
  [ $$status -eq 0 ] || exit 1; \
  printf '%s\n' "$$@" | $(call check_list_overlaps,$(1)) >&2 || exit 1; \
  found=`$(ocamltest) $$(printf ' -find-test-dirs %s' "$$@")` || exit 1; \
  printf '%s\n' "$$found" | $(call check_list_tests,$(1)) >&2

# The directories directly under $(DIR), tests by default, that have tests
# in them or below, one per line: the names that a list of all these tests
# is made of. A directory without tests is not among them, since a list
# must not name it. $(DIR) is taken as the lists take a name: a ./ before
# it and slashes after it do not count, so that DIR=tests/ gives names of
# the form tests/<directory> too.
test_dirs_parent = printf '%s\n' '$(or $(DIR),tests)' \
  | sed -e 's|^\(\./\)*\(.\)|\2|' -e 's|\(.\)/*$$|\1|'

.PHONY: test-dirs
test-dirs:
	@parent=`$(test_dirs_parent)`; \
	 dirs=`$(ocamltest) -find-test-dirs "$$parent"` || exit 1; \
	 echo "$$dirs" | awk -v parent="$$parent/" ' \
	   index($$0, parent) == 1 { \
	     name = substr($$0, length(parent) + 1); \
	     sub(/\/.*/, "", name); \
	     if (!(name in printed)) { printed[name] = 1; print parent name } \
	   }'

.PHONY: list
list:
	@if [ -z "$(FILE)" ]; \
    then echo "No value set for variable 'FILE'."; \
    exit 1; \
  fi
	@$(MAKE) --no-print-directory one LIST="$(FILE)" ONELOG="$(TESTLOG)"

.PHONY: list-parallel
list-parallel: lib tools
	@if [ -z "$(FILE)" ]; \
	  then echo "No value set for variable 'FILE'."; \
	  exit 1; \
	fi
	@echo | parallel >/dev/null 2>/dev/null \
	 || (echo "Unable to run the GNU parallel tool;";\
	     echo "You should install it before using the parallel* targets.";\
	     exit 1)
	@echo | parallel --gnu --no-notice >/dev/null 2>/dev/null \
	 || (echo "Your 'parallel' tool seems incompatible with GNU parallel.";\
	     echo "This target requires GNU parallel.";\
	     exit 1)
	@$(call check_list,$(FILE))
	@rm -f $(failstamp)
	@$(call or_stamp,listing the tests,$(ocamltest) \
	    $(addprefix -find-tests ,\
      $(shell { $(call list_dirs,$(FILE)); } 2>/dev/null))) \
	 | $(call or_stamp,GNU parallel,\
	     parallel --gnu --no-notice --keep-order $(J_ARGUMENT) \
	     "$(MAKE) $(NO_PRINT) exec-test-file TEST={} 2>&1") \
	 | tee $(TESTLOG)
	@$(MAKE) check-failstamp
	@$(MAKE) report

.PHONY: one
one:
	@case "$(words $(DIR) $(LIST) $(TEST))" in \
   0) echo 'No value set for variable DIR, LIST or TEST'>&2; exit 1;; \
   1) exit 0;; \
   *) echo 'Please specify exactly one of DIR, LIST or TEST'>&2; exit 1;; \
   esac
	@if [ -n '$(DIR)' ] && [ ! -d '$(DIR)' ]; then \
    echo "Directory '$(DIR)' does not exist."; exit 1; \
  fi
	@if [ -n '$(TEST)' ] && [ ! -e '$(TEST)' ]; then \
    echo "Test '$(TEST)' does not exist."; exit 1; \
  fi
	@if [ -n '$(LIST)' ]; then $(call check_list,$(LIST)); fi
	@rm -f $(failstamp)
	@if [ -n '$(DIR)' ] ; then \
    $(MAKE) --no-print-directory exec-one DIR=$(DIR) \
      2>&1 | tee $(ONELOG) ; \
   fi
	@if [ -n '$(TEST)' ] ; then \
    { TERM=dumb $(OCAMLTESTENV) $(ocamltest) $(OCAMLTESTFLAGS) $(TEST) || \
      echo " ... testing '$(notdir $(TEST))' => unexpected error"; } \
     2>&1 | tee $(ONELOG); fi
	@if [ -n '$(LIST)' ] ; then \
     for dir in `$(call list_dirs,$(LIST))`; do \
       $(MAKE) --no-print-directory exec-one DIR="$$dir" ; \
     done 2>&1 | tee $(ONELOG) ; \
   fi
	@$(MAKE) check-failstamp
	@$(MAKE) TESTLOG=$(ONELOG) report

.PHONY: exec-one
exec-one:
	@$(call or_stamp,listing the directories of tests in $(DIR),\
	    $(ocamltest) -find-test-dirs $(DIR)) \
	 | while $(IFS_LINE) read -r dir; do \
	  echo "Running tests from '$$dir' ..."; \
	  $(MAKE) exec-ocamltest DIR="$$dir" \
	    OCAMLTESTENV="OCAMLTESTDIR=$(OCAMLTESTDIR)"; \
	done

.PHONY: exec-ocamltest
exec-ocamltest:
	@if [ -z "$(DIR)" ]; then exit 1; fi
	@if [ ! -d "$(DIR)" ]; then exit 1; fi
	@($(call or_stamp,listing the tests of $(DIR),\
	    $(ocamltest) -list-tests $(DIR)) \
	 | while $(IFS_LINE) read -r testfile; do \
	   TERM=dumb $(OCAMLTESTENV) \
	     $(ocamltest) $(OCAMLTESTFLAGS) "$(DIR)/$$testfile" || \
	   echo " ... testing '$$testfile' => unexpected error"; \
	done) || echo directory "$(DIR)" >>$(failstamp)

.PHONY: exec-test-file
exec-test-file: test_dir = $(patsubst %/,%,$(dir $(TEST)))
exec-test-file:
	@echo "Running tests from '$(test_dir)' ..."
	@TERM=dumb OCAMLTESTDIR=$(BASEDIR_HOST)/$(test_dir)/$(ocamltestdir_name) \
	   $(ocamltest) $(OCAMLTESTFLAGS) $(TEST) || \
	 echo " ... testing '$(notdir $(TEST))' => unexpected error"

.PHONY: clean-one
clean-one:
	@if [ ! -f $(DIR)/Makefile ]; then \
	  for dir in $(DIR)/*; do \
	    if [ -d $$dir ]; then \
	      $(MAKE) clean-one DIR=$$dir; \
	    fi; \
	  done; \
	else \
	  cd $(DIR) && $(MAKE) TERM=dumb BASEDIR=$(BASEDIR) clean; \
	fi

# Reference files are shared by every codegen variant. Promoting under
# CODEGEN_VARIANT is how tests that only run under a variant get their
# references, so it is allowed, and announced.
.PHONY: announce-promotion
announce-promotion:
ifneq "$(CODEGEN_VARIANT)" ""
	@echo "Promoting with CODEGEN_VARIANT=$(CODEGEN_VARIANT):" \
	  "reference files are shared by every codegen variant."
endif

.PHONY: promote
promote: announce-promotion
	@$(MAKE) one PROMOTE=true

.PHONY: clean
clean:
	find . -name '*_ocamltest*' | xargs rm -rf
	rm -f $(failstamp)

.PHONY: distclean
distclean: clean
	rm -f _log*

.PHONY: report
report:
	@if [ ! -f $(TESTLOG) ]; then echo "No $(TESTLOG) file."; exit 1; fi
	@$(AWK) -f ./summarize.awk < $(TESTLOG)

.PHONY: promote-failed
promote-failed: lib tools announce-promotion
	@if [ ! -f $(TESTLOG) ]; then echo "No $(TESTLOG) file."; exit 1; fi
	@for test in \
	     $$(awk -f ./summarize.awk -v only_report_failed_tests=1 < $(TESTLOG)); \
	 do \
	   $(MAKE) one PROMOTE=true TEST="$$test"; \
	 done
