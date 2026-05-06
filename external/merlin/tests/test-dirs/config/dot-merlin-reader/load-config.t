  $ cat > .merlin <<EOF
  > B build/dir
  > S source/dir
  > BH build-hidden/dir
  > SH source-hidden/dir
  > STDLIB /stdlib
  > WRAPPING_PREFIX Prefix__
  > UNIT_NAME_FOR a Prefix__a
  > UNIT_NAME_FOR b b
  > UNIT_NAME_FOR c    Prefix_c
  > SOURCE_ROOT /root
  > EOF

  $ FILE=$(pwd)/test.ml; dot-merlin-reader <<EOF | sed 's#[0-9]*:#?:#g'
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:B?:$TESTCASE_ROOT/build/dir)(?:S?:$TESTCASE_ROOT/source/dir)(?:BH?:$TESTCASE_ROOT/build-hidden/dir)(?:SH?:$TESTCASE_ROOT/source-hidden/dir)(?:WRAPPING_PREFIX?:Prefix__)(?:UNIT_NAME_FOR(?:a?:Prefix__a))(?:UNIT_NAME_FOR(?:b?:b))(?:UNIT_NAME_FOR(?:c?:Prefix_c))(?:STDLIB?:/stdlib)(?:SOURCE_ROOT?:/root))

Use ocamlmerlin instead of $MERLIN for this test because $MERLIN configures the stdlib,
but we want to observe the stdlib being configured via the STDLIB directive.
  $ echo | ocamlmerlin single dump-configuration -filename test.ml 2> /dev/null | jq '.value.merlin'
  {
    "build_path": [
      "$TESTCASE_ROOT/build/dir"
    ],
    "source_path": [
      "$TESTCASE_ROOT/source/dir"
    ],
    "hidden_build_path": [
      "$TESTCASE_ROOT/build-hidden/dir"
    ],
    "hidden_source_path": [
      "$TESTCASE_ROOT/source-hidden/dir"
    ],
    "cmi_path": [],
    "cmt_path": [],
    "index_files": [],
    "flags_applied": [],
    "extensions": [],
    "suffixes": [
      {
        "impl": ".ml",
        "intf": ".mli"
      },
      {
        "impl": ".re",
        "intf": ".rei"
      }
    ],
    "stdlib": "/stdlib",
    "source_root": "/root",
    "unit_name": null,
    "unit_name_for": {
      "c": "Prefix_c",
      "b": "b",
      "a": "Prefix__a"
    },
    "wrapping_prefix": "Prefix__",
    "reader": [],
    "protocol": "json",
    "log_file": null,
    "log_sections": [],
    "flags_to_apply": [],
    "failures": [],
    "assoc_suffixes": [
      {
        "extension": ".re",
        "reader": "reason"
      },
      {
        "extension": ".rei",
        "reader": "reason"
      }
    ],
    "cache_lifespan": "5"
  }

  $ rm .merlin
