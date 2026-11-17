(* TEST
(* Tests the -expose and -open-h flags.

   [-expose <module> <name>] lets the source of the compiled unit refer to the
   compilation unit [<module>], typically a hidden dependency, as [<name>].
   The unit's .cmi is unaffected: it mentions the module under its real name.
   Under [-short-paths], which every compilation below uses, printed types may
   use [<name>], so that, as for [-i], they are valid under the same flags.

   [-open-h <module>] opens the compilation unit [<module>], which stays
   hidden if it is a hidden dependency.

   Module [Lib__bar] (in subdirectory [lib]) is a hidden dependency, via
   [-H lib]; the source refers to a module [Foo] that only exists via
   [-expose Lib__bar Foo]. *)

 subdirectories = "lib";
 readonly_files =
   "use_foo.ml consumer.ml withmli.mli withmli.ml consumer_mli.ml \
    collide.ml collide_consumer.ml dup_expose.ml open_foo.ml \
    mention_only.ml mention_only_consumer.ml misspelt.ml old_name.ml \
    hide.ml wrap.ml expose_after_open.ml \
    open_after_expose.ml vis.ml cmimod.mli order_open.ml order_open_cmi.ml \
    order_open_h.ml order_expose.ml";
 ocamlc_flags = "-short-paths";

 setup-ocamlc.byte-build-env;
 flags = "-nostdlib -nopervasives -I lib";
 module = "lib/lib__bar.ml";
 ocamlc.byte;

 (* [-expose Lib__bar Foo] lets [use_foo.ml] refer to [Foo]. *)
 flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Foo";
 module = "use_foo.ml";
 ocamlc.byte;

 (* A consumer compiled WITHOUT -expose sees [Use_foo.y : Lib__bar.t]. *)
 flags = "-nostdlib -nopervasives -I lib";
 module = "consumer.ml";
 ocamlc.byte;

 {
   (* [-i] under [-expose] prints [val y : Foo.t], which is valid under the
      same flags, rather than [val y : Lib__bar.t]. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Foo -i";
   module = "use_foo.ml";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/expose_i.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   (* Without -expose, [Foo] is unbound. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib";
   module = "use_foo.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference =
     "${test_source_directory}/unbound_foo.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   (* The module stays hidden under its own name. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Foo";
   module = "old_name.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/old_name.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   (* ... unless it is exposed under its own name. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Lib__bar";
   module = "old_name.ml";
   ocamlc.byte;
 }{
   (* The module may also be visible, in which case it stays visible under
      its own name. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib -expose Lib__bar Foo";
   module = "use_foo.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib -open-h Lib__bar";
   module = "hide.ml";
   ocamlc.byte;
 }{
   (* Separate .mli and .ml, both compiled with -expose: the name resolves on
      both sides, and the implementation matches the interface. A consumer
      then uses [Withmli.w] without any flag. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Foo";
   module = "withmli.mli";
   ocamlc.byte;
   module = "withmli.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib";
   module = "consumer_mli.ml";
   ocamlc.byte;
 }{
   (* The unit defines its own [Foo], which shadows [-expose Lib__bar Foo]. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Foo";
   module = "collide.ml";
   ocamlc.byte;
   (* A consumer (no -expose) sees [Collide.Foo] as the unit's own generative
      functor. *)
   flags = "-nostdlib -nopervasives -I lib";
   module = "collide_consumer.ml";
   ocamlc.byte;
 }{
   (* Repeated [-expose] under the same name: the last one wins, so [Foo]
      resolves to [Lib__baz] and [Foo.only_baz] (present only in [Lib__baz])
      type-checks. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   module = "lib/lib__baz.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Foo \
            -expose Lib__baz Foo";
   module = "dup_expose.ml";
   ocamlc.byte;
 }{
   (* [-expose] and [-open] are processed in command-line order: an [-open]
      can refer to a name exposed before it... *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Foo -open Foo";
   module = "open_foo.ml";
   ocamlc.byte;
 }{
   (* ... but not to one exposed after it. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -open Foo -expose Lib__bar Foo";
   module = "open_foo.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference =
     "${test_source_directory}/open_before_expose.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   (* [-open], [-open-cmi], [-open-h] and [-expose] are processed in
      command-line order: each one brings a module [M] into scope, and the
      last one shadows the others, whichever it is. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__hid.ml";
   ocamlc.byte;
   module = "lib/lib__exp.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives";
   module = "vis.ml";
   ocamlc.byte;
   module = "cmimod.mli";
   ocamlc.byte;
   {
     flags = "-nostdlib -nopervasives -H lib -expose Lib__exp M \
              -open-h Lib__hid -open-cmi cmimod.cmi -open Vis";
     module = "order_open.ml";
     ocamlc.byte;
   }{
     flags = "-nostdlib -nopervasives -H lib -open Vis -expose Lib__exp M \
              -open-h Lib__hid -open-cmi cmimod.cmi";
     module = "order_open_cmi.ml";
     ocamlc.byte;
   }{
     flags = "-nostdlib -nopervasives -H lib -open-cmi cmimod.cmi -open Vis \
              -expose Lib__exp M -open-h Lib__hid";
     module = "order_open_h.ml";
     ocamlc.byte;
   }{
     flags = "-nostdlib -nopervasives -H lib -open-h Lib__hid \
              -open-cmi cmimod.cmi -open Vis -expose Lib__exp M";
     module = "order_expose.ml";
     ocamlc.byte;
   }
 }{
   (* An exposed name shadows a module of the same name opened before it, and
      is shadowed by one opened after it. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives";
   module = "wrap.ml";
   ocamlc.byte;
   {
     flags = "-nostdlib -nopervasives -H lib -open Wrap -expose Lib__bar Foo";
     module = "expose_after_open.ml";
     ocamlc.byte;
   }{
     flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Foo -open Wrap";
     module = "open_after_expose.ml";
     ocamlc.byte;
   }
 }{
   (* [Foo] is only mentioned, under [-no-alias-deps]: a consumer still
      resolves the module. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -no-alias-deps -expose Lib__bar Foo";
   module = "mention_only.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib";
   module = "mention_only_consumer.ml";
   ocamlc.byte;
 }{
   (* Exposed names are offered as spelling suggestions. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -expose Lib__bar Foo";
   module = "misspelt.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/misspelt.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   (* [-open-h Lib__bar]: the contents of [Lib__bar] are in scope, but
      [Lib__bar] itself stays hidden. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/lib__bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -H lib -open-h Lib__bar";
   module = "hide.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/hide.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   (* The exposed module must be a compilation unit, not a path. *)
   setup-ocamlc.byte-build-env;
   compiler_output = "expose.raw";
   flags = "-nostdlib -nopervasives -H lib -expose Outer.Lib__bar Foo";
   module = "use_foo.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "sh ${test_source_directory}/strip_program_name.sh expose.raw";
   output = "compiler-output";
   script;
   compiler_output = "compiler-output";
   compiler_reference =
     "${test_source_directory}/expose_not_unit.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   (* Likewise for [-open-h]. *)
   setup-ocamlc.byte-build-env;
   compiler_output = "open-h.raw";
   flags = "-nostdlib -nopervasives -H lib -open-h Outer.Lib__bar";
   module = "use_foo.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "sh ${test_source_directory}/strip_program_name.sh open-h.raw";
   output = "compiler-output";
   script;
   compiler_output = "compiler-output";
   compiler_reference =
     "${test_source_directory}/open_h_not_unit.ocamlc.reference";
   check-ocamlc.byte-output;
 }
*)
