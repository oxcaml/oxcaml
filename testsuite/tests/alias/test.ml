(* TEST
(* Tests the -alias flag.

   [-alias <from> <to>] adds a hidden [module <from> = <to>] alias to the
   compiled unit, so the source may refer to [<from>] as the persistent
   module [<to>]. The alias is hidden: it is recorded in the .cmi (so a
   consumer can resolve types that mention [<from>]) but omitted from the
   interface printed by [-i].

   Module [Bar] (in subdirectory [lib]) plays the role of [<to>]; the source
   refers to a module [Foo] that only exists via [-alias Foo Bar]. *)

 subdirectories = "lib";
 readonly_files =
   "use_foo.ml consumer.ml withmli.mli withmli.ml consumer_mli.ml \
    collide.ml collide_consumer.ml dup_alias.ml \
    carrier.ml leak.ml leak_consumer.ml dup_real.ml";

 setup-ocamlc.byte-build-env;
 flags = "-nostdlib -nopervasives -I lib";
 module = "lib/bar.ml";
 ocamlc.byte;

 (* [-alias Foo Bar] lets [use_foo.ml] refer to [Foo]. *)
 flags = "-nostdlib -nopervasives -I lib -alias Foo Bar";
 module = "use_foo.ml";
 ocamlc.byte;

 (* A consumer compiled WITHOUT -alias still resolves [Use_foo.y], because the
    hidden [module Foo = Bar] recorded in [use_foo.cmi] makes [Foo.t] resolve
    to [Bar.t]. *)
 flags = "-nostdlib -nopervasives -I lib";
 module = "consumer.ml";
 ocamlc.byte;

 {
   (* [-i] under [-alias] prints [val y : Foo.t] but omits the hidden
      [module Foo = Bar] declaration. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib -alias Foo Bar -i";
   module = "use_foo.ml";
   ocamlc.byte;
   compiler_reference = "${test_source_directory}/alias_i.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   (* Without -alias, [Foo] is unbound. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/bar.ml";
   ocamlc.byte;
   module = "use_foo.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   compiler_reference =
     "${test_source_directory}/unbound_foo.ocamlc.reference";
   check-ocamlc.byte-output;
 }{
   (* Separate .mli and .ml, both compiled with -alias: the alias resolves on
      both sides, and the implementation matches the interface (the aliases
      injected into each must denote the same module). A consumer then resolves
      [Withmli.w] without any flag. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib -alias Foo Bar";
   module = "withmli.mli";
   ocamlc.byte;
   module = "withmli.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib";
   module = "consumer_mli.ml";
   ocamlc.byte;
 }{
   (* The unit defines its own [Foo], colliding by name with [-alias Foo Bar].
      The real [Foo] shadows the injected alias instead of leaving a duplicate-
      named signature, so this compiles cleanly. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib -alias Foo Bar";
   module = "collide.ml";
   ocamlc.byte;
   (* A consumer (no -alias) sees [Collide.Foo] as the unit's own generative
      functor -- the visible component wins over the hidden alias. *)
   flags = "-nostdlib -nopervasives -I lib";
   module = "collide_consumer.ml";
   ocamlc.byte;
 }{
   (* Repeated [-alias] for the same name: the last one wins, so [Foo] resolves
      to [Baz] and [Foo.only_baz] (present only in [Baz]) type-checks. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/bar.ml";
   ocamlc.byte;
   module = "lib/baz.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib -alias Foo Bar -alias Foo Baz";
   module = "dup_alias.ml";
   ocamlc.byte;
 }{
   (* [Carrier], compiled with [-alias Foo Bar], records a hidden
      [module Foo = Bar] in its .cmi. [leak.ml] [include]s [Carrier] -- so it
      inherits that hidden alias -- and also defines its own [Foo]. The two
      must coexist (no "Multiple definition", no broken inclusion check). A
      consumer then resolves both the included value and [Leak.Foo.x]. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib -alias Foo Bar";
   module = "carrier.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib";
   module = "leak.ml";
   ocamlc.byte;
   module = "leak_consumer.ml";
   ocamlc.byte;
 }{
   (* Two real modules of the same name still clash, even with [-alias]: hidden
      aliases coexisting must not weaken uniqueness of visible components. *)
   setup-ocamlc.byte-build-env;
   flags = "-nostdlib -nopervasives -I lib";
   module = "lib/bar.ml";
   ocamlc.byte;
   flags = "-nostdlib -nopervasives -I lib -alias Foo Bar";
   module = "dup_real.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }
*)
