Test using findlib with .merlin files.

  $ show_config () {
  >   config="$(PATH="$PWD/bin:$PATH" $MERLIN single dump-configuration -filename test.ml 2> /dev/null | jq .value)"
  >   echo "ppx: $(echo "$config" | jq .ocaml.ppx)"
  >   echo "source path: $(echo "$config" | jq .merlin.source_path)"
  >   echo "build path: $(echo "$config" | jq .merlin.build_path)"
  >   echo "failures: $(echo "$config" | jq .merlin.failures)"
  > }

Setup a test project.

  $ mkdir -p lib/foo lib/bar bin

  $ cat > lib/bar/META <<EOF
  > version = "1"
  > ppx = "./ppx_bar"
  > EOF

  $ cat > lib/foo/META <<EOF
  > version = "1"
  > requires = "bar"
  > ppx = "ppx_foo"
  > ppxopt = "ppx_bar,-for-bar,@bar/plugin.cma,+compiler-libs/x.cma ppx_foo,-for-foo"
  > EOF

  $ cat > findlib.conf <<EOF
  > stdlib="/stdlib"
  > EOF

Basic .merlin file.

  $ cat > .merlin <<EOF
  > FINDLIB findlib.conf
  > FINDLIB_PATH lib
  > PKG foo
  > EOF

  $ show_config
  ppx: [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "$TESTCASE_ROOT/lib/bar/./ppx_bar -for-bar $TESTCASE_ROOT/lib/bar/plugin.cma /stdlib/compiler-libs/x.cma"
    },
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "ppx_foo -for-foo"
    }
  ]
  source path: [
    "$TESTCASE_ROOT/lib/bar",
    "$TESTCASE_ROOT/lib/foo"
  ]
  build path: [
    "$TESTCASE_ROOT/lib/bar",
    "$TESTCASE_ROOT/lib/foo"
  ]
  failures: []

Optional library that isn't present is quietly ignored.

  $ cat > .merlin <<EOF
  > FINDLIB findlib.conf
  > FINDLIB_PATH lib
  > PKG foo not-installed?
  > EOF

  $ show_config
  ppx: [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "$TESTCASE_ROOT/lib/bar/./ppx_bar -for-bar $TESTCASE_ROOT/lib/bar/plugin.cma /stdlib/compiler-libs/x.cma"
    },
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "ppx_foo -for-foo"
    }
  ]
  source path: [
    "$TESTCASE_ROOT/lib/bar",
    "$TESTCASE_ROOT/lib/foo"
  ]
  build path: [
    "$TESTCASE_ROOT/lib/bar",
    "$TESTCASE_ROOT/lib/foo"
  ]
  failures: []

Optional library that is present is used.

  $ cat > .merlin <<EOF
  > FINDLIB findlib.conf
  > FINDLIB_PATH lib
  > PKG foo?
  > EOF

  $ show_config
  ppx: [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "$TESTCASE_ROOT/lib/bar/./ppx_bar -for-bar $TESTCASE_ROOT/lib/bar/plugin.cma /stdlib/compiler-libs/x.cma"
    },
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "ppx_foo -for-foo"
    }
  ]
  source path: [
    "$TESTCASE_ROOT/lib/bar",
    "$TESTCASE_ROOT/lib/foo"
  ]
  build path: [
    "$TESTCASE_ROOT/lib/bar",
    "$TESTCASE_ROOT/lib/foo"
  ]
  failures: []

Missing packages are reported without dropping the ones that were found.

  $ cat > .merlin <<EOF
  > FINDLIB findlib.conf
  > FINDLIB_PATH lib
  > PKG bar not-installed
  > EOF

  $ show_config
  ppx: [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "$TESTCASE_ROOT/lib/bar/./ppx_bar"
    }
  ]
  source path: [
    "$TESTCASE_ROOT/lib/bar"
  ]
  build path: [
    "$TESTCASE_ROOT/lib/bar"
  ]
  failures: [
    "Failed to load packages: not-installed"
  ]

Without FINDLIB_PATH the packages are not visible.

  $ cat > .merlin <<EOF
  > PKG foo
  > EOF

  $ show_config
  ppx: []
  source path: []
  build path: []
  failures: [
    "Failed to load packages: foo"
  ]

Non-existing findling config file is reported.

  $ cat > .merlin <<EOF
  > FINDLIB no-such.conf
  > PKG foo
  > EOF

  $ show_config
  ppx: []
  source path: []
  build path: []
  failures: [
    "ocamlfind: Config file not found - neither $TESTCASE_ROOT/no-such.conf nor the directory $TESTCASE_ROOT/no-such.conf.d"
  ]

Other directives are respected with findlib isn't available.

  $ cat > .merlin <<EOF
  > B baz
  > S baz
  > FINDLIB findlib.conf
  > FINDLIB_PATH lib
  > PKG foo
  > EOF

  $ path_without_ocamlfind="$(echo "$PATH" | tr ':' '\n' \
  >   | grep -vxF "$(dirname "$(command -v ocamlfind)")" | paste -sd:)"
  $ (PATH="$path_without_ocamlfind"; show_config)
  ppx: []
  source path: [
    "$TESTCASE_ROOT/baz"
  ]
  build path: [
    "$TESTCASE_ROOT/baz"
  ]
  failures: [
    "Cannot run ocamlfind: No such file or directory"
  ]
