  $ mkdir test

  $ test_merlin_overrides () {
  > local position="$1"
  > local file="$2"
  > 
  > local locate_output=$($MERLIN single locate -position "$position" -filename "$file" < "$file" | \
  >   jq -r .value)
  > 
  > local document_output=$($MERLIN single document -position "$position" -filename "$file" < "$file" | \
  >   jq -r .value)
  > 
  > echo "[merlin locate] output: $locate_output" 
  > echo "[merlin document] output: $document_output" 
  > }

Test no .merlin, relative path

  $ cat >./test/simple.ml <<EOF
  > [@@@do_nothing]
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "simple.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 4 };
  >          loc_end =
  >            { pos_fname = "simple.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "@@@do_nothing expands into nothing"
  >    }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "simple.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 4 };
  >          loc_end =
  >            { pos_fname = "simple.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/ppx.ml";
  >          pos_lnum = 101;
  >          pos_bol = 2833;
  >          pos_cnum = 2854
  >        }
  >    }]]
  > EOF

  $ test_merlin_overrides "1:4" "./test/simple.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 101,
      "col": 21
    }
  }
  [merlin document] output: @@@do_nothing expands into nothing

All following tests are performed in /test and merlin has access to /test/.merlin

  $ cd test
  $ cat >.merlin <<EOF
  > SOURCE_ROOT ../
  > EOF

Happy path .ml and .mli tests

  $ cat >basic.ml <<EOF
  > let f a b c d = a - b + c - d
  > let _ = [%swap f 1 2] 3 4
  > let _ = (0 [@add_one]) + 2
  > 
  > [@@@do_nothing]
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "basic.ml"; pos_lnum = 3; pos_bol = 56; pos_cnum = 69
  >            };
  >          loc_end =
  >            { pos_fname = "basic.ml"; pos_lnum = 3; pos_bol = 56; pos_cnum = 76
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "@add_one expands expressions with a '+ 1'"
  >    };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "basic.ml"; pos_lnum = 5; pos_bol = 84; pos_cnum = 88
  >           };
  >         loc_end =
  >           { pos_fname = "basic.ml"; pos_lnum = 5; pos_bol = 84; pos_cnum = 98
  >           };
  >         loc_ghost = false
  >       };
  >     payload = "@@@do_nothing expands into nothing"
  >   };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "basic.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 40
  >           };
  >         loc_end =
  >           { pos_fname = "basic.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 44
  >           };
  >         loc_ghost = false
  >       };
  >     payload = "%swap swaps the first two arguments of a function call"
  >   }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "basic.ml"; pos_lnum = 3; pos_bol = 56; pos_cnum = 69
  >            };
  >          loc_end =
  >            { pos_fname = "basic.ml"; pos_lnum = 3; pos_bol = 56; pos_cnum = 76
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/ppx.ml";
  >          pos_lnum = 53;
  >          pos_bol = 1612;
  >          pos_cnum = 1633
  >        }
  >    };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "basic.ml"; pos_lnum = 5; pos_bol = 84; pos_cnum = 88
  >           };
  >         loc_end =
  >           { pos_fname = "basic.ml"; pos_lnum = 5; pos_bol = 84; pos_cnum = 98
  >           };
  >         loc_ghost = false
  >       };
  >     payload =
  >       {
  >         pos_fname =
  >           "test/ppx.ml";
  >         pos_lnum = 101;
  >         pos_bol = 2833;
  >         pos_cnum = 2854
  >       }
  >   };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "basic.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 40
  >           };
  >         loc_end =
  >           { pos_fname = "basic.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 44
  >           };
  >         loc_ghost = false
  >       };
  >     payload =
  >       {
  >         pos_fname =
  >           "test/ppx.ml";
  >         pos_lnum = 12;
  >         pos_bol = 336;
  >         pos_cnum = 360
  >       }
  >   }]]
  > EOF

  $ cat >basic.mli <<EOF
  > val f : int -> int -> int -> int -> int [@@identity]
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "basic.mli"; pos_lnum = 1; pos_bol = 0; pos_cnum = 43
  >            };
  >          loc_end =
  >            { pos_fname = "basic.mli"; pos_lnum = 1; pos_bol = 0; pos_cnum = 51
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "@identity does not expand into anything"
  >    }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "basic.mli"; pos_lnum = 1; pos_bol = 0; pos_cnum = 43
  >            };
  >          loc_end =
  >            { pos_fname = "basic.mli"; pos_lnum = 1; pos_bol = 0; pos_cnum = 51
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/ppx.ml";
  >          pos_lnum = 79;
  >          pos_bol = 2317;
  >          pos_cnum = 2338
  >        }
  >    }]]
  > EOF

Test overrides on %swap

  $ test_merlin_overrides "2:10" "./basic.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 12,
      "col": 24
    }
  }
  [merlin document] output: %swap swaps the first two arguments of a function call

Test overrides on last character of %swap

  $ test_merlin_overrides "2:14" "./basic.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 12,
      "col": 24
    }
  }
  [merlin document] output: %swap swaps the first two arguments of a function call

Test overrides one character to the right of %swap

  $ test_merlin_overrides "2:15" "./basic.ml"
  [merlin locate] output: Not in environment 'f'
  [merlin document] output: Not in environment 'f'

Test overrides on @add_one

  $ test_merlin_overrides "3:13" "./basic.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 53,
      "col": 21
    }
  }
  [merlin document] output: @add_one expands expressions with a '+ 1'

Test overrides on @@@do_nothing

  $ test_merlin_overrides "5:4" "./basic.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 101,
      "col": 21
    }
  }
  [merlin document] output: @@@do_nothing expands into nothing

Test overrides on @@identity

  $ test_merlin_overrides "1:45" "./basic.mli"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 79,
      "col": 21
    }
  }
  [merlin document] output: @identity does not expand into anything

Multiple @@@merlin attributes should be merged and both usable

  $ cat >multiple-attribute.ml <<EOF
  > let f a b c d = a - b + c - d
  > let _ = [%swap f 1 2] 3 4
  > let _ = (0 [@add_one]) + 2
  > 
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "multiple-attribute.ml"; pos_lnum = 3; pos_bol = 56; pos_cnum = 69
  >            };
  >          loc_end =
  >            { pos_fname = "multiple-attribute.ml"; pos_lnum = 3; pos_bol = 56; pos_cnum = 76
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "@add_one expands expressions with a '+ 1'"
  >    }]]
  > [@@@merlin.document
  >   [{
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "multiple-attribute.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 40
  >           };
  >         loc_end =
  >           { pos_fname = "multiple-attribute.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 44
  >           };
  >         loc_ghost = false
  >       };
  >     payload = "%swap swaps the first two arguments of a function call"
  >   }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "multiple-attribute.ml"; pos_lnum = 3; pos_bol = 56; pos_cnum = 69
  >            };
  >          loc_end =
  >            { pos_fname = "multiple-attribute.ml"; pos_lnum = 3; pos_bol = 56; pos_cnum = 76
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/ppx.ml";
  >          pos_lnum = 53;
  >          pos_bol = 1612;
  >          pos_cnum = 1633
  >        }
  >    }]]
  > [@@@merlin.locate
  >   [{
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "multiple-attribute.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 40
  >           };
  >         loc_end =
  >           { pos_fname = "multiple-attribute.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 44
  >           };
  >         loc_ghost = false
  >       };
  >     payload =
  >       {
  >         pos_fname =
  >           "test/ppx.ml";
  >         pos_lnum = 12;
  >         pos_bol = 336;
  >         pos_cnum = 360
  >       }
  >   }]]
  > EOF

  $ test_merlin_overrides "2:10" "./multiple-attribute.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 12,
      "col": 24
    }
  }
  [merlin document] output: %swap swaps the first two arguments of a function call

  $ test_merlin_overrides "3:13" "./multiple-attribute.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 53,
      "col": 21
    }
  }
  [merlin document] output: @add_one expands expressions with a '+ 1'

Attribute location should not affect functionality. 

  $ cat >attribute-at-top.ml <<EOF
  > [@@@merlin.document
  >   [ { location =
  >         { loc_start =
  >             { pos_fname = "attribute-at-top.ml"; pos_lnum = 31; pos_bol = 867; pos_cnum = 880 }
  >         ; loc_end =
  >             { pos_fname = "attribute-at-top.ml"; pos_lnum = 31; pos_bol = 867; pos_cnum = 887 }
  >         ; loc_ghost = false
  >         }
  >     ; payload = "@add_one expands expressions with a '+ 1'"
  >     }
  >   ]]
  > 
  > [@@@merlin.locate
  >   [ { location =
  >         { loc_start =
  >             { pos_fname = "attribute-at-top.ml"; pos_lnum = 31; pos_bol = 867; pos_cnum = 880 }
  >         ; loc_end =
  >             { pos_fname = "attribute-at-top.ml"; pos_lnum = 31; pos_bol = 867; pos_cnum = 887 }
  >         ; loc_ghost = false
  >         }
  >     ; payload =
  >         { pos_fname =
  >             "test/ppx.ml"
  >         ; pos_lnum = 53
  >         ; pos_bol = 1612
  >         ; pos_cnum = 1633
  >         }
  >     }
  >   ]]
  > 
  > let _ = (0 [@add_one]) + 2
  > EOF

  $ test_merlin_overrides "31:13" "./attribute-at-top.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 53,
      "col": 21
    }
  }
  [merlin document] output: @add_one expands expressions with a '+ 1'

@@@merlin attribute's payload has invalid structure. below, the payload is missing

  $ cat >invalid-payload.ml <<EOF
  > let _ = (0 [@add_one]) + 2
  > [@@@merlin.document]
  > [@@@merlin.locate]
  > EOF

  $ test_merlin_overrides "1:13" "./invalid-payload.ml"
  [merlin locate] output: Not in environment 'add_one'
  [merlin document] output: Not in environment 'add_one'

@@@merlin attribute's payload contains two target overrides. the first target should be returned

  $ cat >multiple-overrides.ml <<EOF
  > let _ = (0 [@add_one]) + 2
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "multiple-overrides.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 13
  >            };
  >          loc_end =
  >            { pos_fname = "multiple-overrides.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 20
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "first target document override"
  >    };
  >    {
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "multiple-overrides.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 13
  >            };
  >          loc_end =
  >            { pos_fname = "multiple-overrides.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 20
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "second target document override"
  >    }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "multiple-overrides.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 13
  >            };
  >          loc_end =
  >            { pos_fname = "multiple-overrides.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 20
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/first-override.ml";
  >          pos_lnum = 53;
  >          pos_bol = 1612;
  >          pos_cnum = 1633
  >        }
  >    };
  >   {
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "multiple-overrides.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 13
  >            };
  >          loc_end =
  >            { pos_fname = "multiple-overrides.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 20
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/second-override.ml";
  >          pos_lnum = 53;
  >          pos_bol = 1612;
  >          pos_cnum = 1633
  >        }
  >    }]]
  > EOF

  $ test_merlin_overrides "1:13" "./multiple-overrides.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/first-override.ml",
    "pos": {
      "line": 53,
      "col": 21
    }
  }
  [merlin document] output: first target document override

Override nested PPXs

  $ cat >nested-ppx.ml <<EOF
  > let f a b c d = a - b + c - d
  > let _ = [%swap [%swap f 1 2] 3 4]
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "nested-ppx.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 47
  >            };
  >          loc_end =
  >            { pos_fname = "nested-ppx.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 51
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "inner %swap PPX"
  >    };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "nested-ppx.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 40
  >           };
  >         loc_end =
  >           { pos_fname = "nested-ppx.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 44
  >           };
  >         loc_ghost = false
  >       };
  >     payload = "outer %swap PPX"
  >   }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "nested-ppx.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 47
  >            };
  >          loc_end =
  >            { pos_fname = "nested-ppx.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 51
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/inner-%swap-PPX.ml";
  >          pos_lnum = 12;
  >          pos_bol = 336;
  >          pos_cnum = 360
  >        }
  >    };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "nested-ppx.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 40
  >           };
  >         loc_end =
  >           { pos_fname = "nested-ppx.ml"; pos_lnum = 2; pos_bol = 30; pos_cnum = 44
  >           };
  >         loc_ghost = false
  >       };
  >     payload =
  >       {
  >         pos_fname =
  >           "test/outer-%swap-PPX.ml";
  >         pos_lnum = 12;
  >         pos_bol = 336;
  >         pos_cnum = 360
  >       }
  >   }]]
  > EOF

  $ test_merlin_overrides "2:10" "./nested-ppx.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/outer-%swap-PPX.ml",
    "pos": {
      "line": 12,
      "col": 24
    }
  }
  [merlin document] output: outer %swap PPX

  $ test_merlin_overrides "2:17" "./nested-ppx.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/inner-%swap-PPX.ml",
    "pos": {
      "line": 12,
      "col": 24
    }
  }
  [merlin document] output: inner %swap PPX

  $ cat >ppx-payload.ml <<EOF
  > let _ = [%swap f 1 2] 3 4
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "nested-ppx.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 10
  >            };
  >          loc_end =
  >            { pos_fname = "nested-ppx.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "%swap swaps the first two arguments of a function call"
  >    };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "nested-ppx.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 15 };
  >         loc_end =
  >           { pos_fname = "nested-ppx.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 16 };
  >         loc_ghost = false
  >       };
  >     payload = "f can be a %swap-specific argument"
  >   }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "nested-ppx.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 10
  >            };
  >          loc_end =
  >            { pos_fname = "nested-ppx.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/ppx.ml";
  >          pos_lnum = 12;
  >          pos_bol = 336;
  >          pos_cnum = 360
  >        }
  >    };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "nested-ppx.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 15 };
  >         loc_end =
  >           { pos_fname = "nested-ppx.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 16 };
  >         loc_ghost = false
  >       };
  >     payload =
  >        {
  >          pos_fname =
  >            "test/f.ml";
  >          pos_lnum = 1;
  >          pos_bol = 0;
  >          pos_cnum = 5
  >        }
  >   }]]
  > EOF

  $ test_merlin_overrides "1:10" "./ppx-payload.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 12,
      "col": 24
    }
  }
  [merlin document] output: %swap swaps the first two arguments of a function call

  $ test_merlin_overrides "1:15" "./ppx-payload.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/f.ml",
    "pos": {
      "line": 1,
      "col": 5
    }
  }
  [merlin document] output: f can be a %swap-specific argument

Override a floating attribute

  $ cat >floating_attribute.ml <<EOF
  > [@@@do_nothing]
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "floating_attribute.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 4 };
  >          loc_end =
  >            { pos_fname = "floating_attribute.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "@@@do_nothing expands into nothing"
  >    }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "floating_attribute.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 4 };
  >          loc_end =
  >            { pos_fname = "floating_attribute.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/ppx.ml";
  >          pos_lnum = 101;
  >          pos_bol = 2833;
  >          pos_cnum = 2854
  >        }
  >    }]]
  > EOF

  $ test_merlin_overrides "1:4" "./floating_attribute.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 101,
      "col": 21
    }
  }
  [merlin document] output: @@@do_nothing expands into nothing

Override an attribute in a extension's payload

  $ cat >attribute-as-payload.ml <<EOF
  > let _ = [%swap f (1 [@add_one]) 2] 3 4
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "attribute-as-payload.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 22
  >            };
  >          loc_end =
  >            { pos_fname = "attribute-as-payload.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 29
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "@add_one expands expressions with a '+ 1'"
  >    };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "attribute-as-payload.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 10 };
  >         loc_end =
  >           { pos_fname = "attribute-as-payload.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14 };
  >         loc_ghost = false
  >       };
  >     payload = "%swap swaps the first two arguments of a function call"
  >   }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "attribute-as-payload.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 22
  >            };
  >          loc_end =
  >            { pos_fname = "attribute-as-payload.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 29
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/ppx.ml";
  >          pos_lnum = 53;
  >          pos_bol = 1612;
  >          pos_cnum = 1633
  >        }
  >    };
  >   {
  >     location =
  >       {
  >         loc_start =
  >           { pos_fname = "attribute-as-payload.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 10 };
  >         loc_end =
  >           { pos_fname = "attribute-as-payload.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14 };
  >         loc_ghost = false
  >       };
  >     payload =
  >       {
  >         pos_fname =
  >           "test/ppx.ml";
  >         pos_lnum = 12;
  >         pos_bol = 336;
  >         pos_cnum = 360
  >       }
  >   }]]
  > EOF

  $ test_merlin_overrides "1:10" "./attribute-as-payload.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 12,
      "col": 24
    }
  }
  [merlin document] output: %swap swaps the first two arguments of a function call
 
  $ test_merlin_overrides "1:22" "./attribute-as-payload.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/ppx.ml",
    "pos": {
      "line": 53,
      "col": 21
    }
  }
  [merlin document] output: @add_one expands expressions with a '+ 1'

Existing behavior of non-overridden locations should not be affected. Also, override non-PPX locations

  $ cat >non-ppx.ml <<EOF
  > (** [x] is a variable *)
  > let x = 0
  > 
  > let _ = x + 1
  > let _ = x + 1
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "non-ppx.ml"; pos_lnum = 5; pos_bol = 50; pos_cnum = 58
  >            };
  >          loc_end =
  >            { pos_fname = "non-ppx.ml"; pos_lnum = 5; pos_bol = 50; pos_cnum = 59
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "overridden documentation on [x]"
  >    }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "non-ppx.ml"; pos_lnum = 5; pos_bol = 50; pos_cnum = 58
  >            };
  >          loc_end =
  >            { pos_fname = "non-ppx.ml"; pos_lnum = 5; pos_bol = 50; pos_cnum = 59
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/overridden-location-of-x.ml";
  >          pos_lnum = 53;
  >          pos_bol = 1612;
  >          pos_cnum = 1633
  >        }
  >    }]]
  > EOF

  $ test_merlin_overrides "4:8" "./non-ppx.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/non-ppx.ml",
    "pos": {
      "line": 2,
      "col": 4
    }
  }
  [merlin document] output: [x] is a variable

  $ test_merlin_overrides "5:8" "./non-ppx.ml"
  [merlin locate] output: {
    "file": "$TESTCASE_ROOT/test/overridden-location-of-x.ml",
    "pos": {
      "line": 53,
      "col": 21
    }
  }
  [merlin document] output: overridden documentation on [x]
