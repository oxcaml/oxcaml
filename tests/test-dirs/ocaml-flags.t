Get a list of all flags that ocamlopt.opt understands.
  $ OCAMLOPT="$MERLIN_TEST_OCAML_PATH/bin/ocamlc"
  $ OCAML_FLAGS=$("$OCAMLOPT" --help | grep -oP '(?<=  )-[a-z\-_0-9]+(?= )')

Verify that each flag appears in mconfig.ml. Each flag should either be handled or be in
the list of ignored flags. If a new flag appears in this list, you should either make
Merlin handle it or add it to the list of ignored flags.

  $ is_intentionally_unhandled () {
  >   # These flags are unhandled by Merlin and we really do want to raise an error if
  >   # they are passed.
  >   case "$1" in
  >     "-args" | "-args0" | "-depend")
  >       return 0
  >       ;;
  >     *)
  >       return 1
  >       ;;
  >   esac
  > }

  $ mconfig="../../src/kernel/mconfig.ml"
  $ echo "$OCAML_FLAGS" | while IFS= read -r flag; do
  >   grep -q "\"$flag\"" "$mconfig" || \
  >     is_intentionally_unhandled "$flag" || \
  >     echo "Unhandled flag: $flag"
  > done

This list is printed to ensure that a change to the help text does not break the parsing
logic.
  $ echo "$OCAML_FLAGS"
  -a
  -alert
  -absname
  -no-absname
  -locs
  -no-locs
  -annot
  -as-argument-for
  -as-parameter
  -bin-annot
  -bin-annot-cms
  -bin-annot-occurrences
  -c
  -cc
  -cclib
  -ccopt
  -cmi-file
  -color
  -error-style
  -compat-32
  -config
  -config-var
  -custom
  -disable-all-extensions
  -only-erasable-extensions
  -dllib
  -dllpath
  -dtypes
  -extension
  -no-extension
  -extension-universe
  -for-pack
  -g
  -no-g
  -stop-after
  -i
  -impl
  -instantiate
  -intf
  -intf-suffix
  -intf_suffix
  -keep-docs
  -no-keep-docs
  -keep-locs
  -no-keep-locs
  -labels
  -linkall
  -llvm-backend
  -make-runtime
  -make_runtime
  -modern
  -alias-deps
  -no-alias-deps
  -app-funct
  -no-app-funct
  -directory
  -no-check-prims
  -noassert
  -noautolink
  -nolabels
  -nostdlib
  -no-auto-include-otherlibs
  -nocwd
  -nopervasives
  -o
  -opaque
  -open
  -output-obj
  -output-complete-obj
  -output-complete-exe
  -pack
  -parameter
  -pp
  -ppx
  -plugin
  -principal
  -no-principal
  -rectypes
  -no-rectypes
  -runtime-variant
  -with-runtime
  -without-runtime
  -safe-string
  -safer-matching
  -short-paths
  -strict-sequence
  -no-strict-sequence
  -strict-formats
  -no-strict-formats
  -thread
  -unboxed-types
  -no-unboxed-types
  -ddebug-uids
  -ddebug-uid-tables
  -unsafe
  -use-runtime
  -use_runtime
  -v
  -verbose
  -verbose-types
  -no-verbose-types
  -version
  --version
  -vmthread
  -vnum
  -w
  -warn-error
  -warn-help
  -where
  -match-context-rows
  -use-prims
  -shape-format
  -dno-unique-ids
  -dunique-ids
  -dno-locations
  -dlocations
  -dsource
  -dparsetree
  -dtypedtree
  -dshape
  -drawlambda
  -dslambda
  -dlambda
  -dblambda
  -dletreclambda
  -dinstr
  -dcamlprimc
  -dtimings
  -dtimings-precision
  -dcounters
  -dprofile
  -dgranularity
  -dprofile-output
  -dump-into-file
  -dump-into-csv
  -dump-dir
  -debug-ocaml
  -args
  -args0
  -depend
  -help
  --help
