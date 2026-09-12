(* TEST
 (* Bound_globals with parameterised arguments — see each case below. *)

 readonly_files = "\
   foo_q.mli foo_q.ml foo_q__.ml \
   nested_arg.mli nested_arg.ml nested_arg__.ml \
   pair_pq.mli pair_pq.ml pair_pq__.ml \
   partial_pq.mli partial_pq.ml partial_pq__.ml \
   main_nested_arg.ml test_functorize_nested_arg.reference \
   main_partial_pq.ml test_functorize_partial_pq.reference \
   r.mli r__.ml \
   r_impl.mli r_impl.ml r_impl__.ml \
   r_int.mli r_int.ml \
   foo_r.mli foo_r.ml foo_r__.ml \
   nested_r.mli nested_r.ml nested_r__.ml \
   main_nested_r.ml test_functorize_nested_r.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int q q_impl foo_q nested_arg pair_pq \
                 partial_pq bundle_nested bundle_partial \
                 r r_int r_impl foo_r nested_r bundle_nested_r";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../../dunelike/p_int.mli \
        ${test_source_directory}/../../dunelike/p_int.ml \
        ${test_source_directory}/../../dunelike/p_int__.ml";
 dst = "p_int/";
 copy;

 src = "${test_source_directory}/../../dunelike/q.mli \
        ${test_source_directory}/../../dunelike/q__.ml";
 dst = "q/";
 copy;

 src = "${test_source_directory}/../../dunelike/q_impl.ml \
        ${test_source_directory}/../../dunelike/q_impl__.ml";
 dst = "q_impl/";
 copy;

 src = "foo_q.mli foo_q.ml foo_q__.ml";
 dst = "foo_q/";
 copy;

 src = "nested_arg.mli nested_arg.ml nested_arg__.ml";
 dst = "nested_arg/";
 copy;

 src = "pair_pq.mli pair_pq.ml pair_pq__.ml";
 dst = "pair_pq/";
 copy;

 src = "partial_pq.mli partial_pq.ml partial_pq__.ml";
 dst = "partial_pq/";
 copy;

 src = "r.mli r__.ml";
 dst = "r/";
 copy;

 src = "r_int.mli r_int.ml";
 dst = "r_int/";
 copy;

 src = "r_impl.mli r_impl.ml r_impl__.ml";
 dst = "r_impl/";
 copy;

 src = "foo_r.mli foo_r.ml foo_r__.ml";
 dst = "foo_r/";
 copy;

 src = "nested_r.mli nested_r.ml nested_r__.ml";
 dst = "nested_r/";
 copy;

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Parameter P and argument P_int. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H p -open-cmi p/p__.cmi";
 module = "p/p.mli";
 ocamlc.byte;

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -H p_int -open-cmi p_int/p_int__.cmi";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* Parameter Q and argument Q_int. *)

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H q -open-cmi q/q__.cmi";
 module = "q/q.mli";
 ocamlc.byte;

 (* Q_impl: -parameter P -as-argument-for Q — a parameterised argument. *)

 flags = "$flg_int_iface";
 module = "q_impl/q_impl__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for Q -parameter P -I p -I q -H q_impl \
   -open-cmi q_impl/q_impl__.cmi";
 module = "q_impl/q_impl.ml";
 ocamlc.byte;

 {
   (* ===== Case 1: Nested_arg — Foo_q[Q:Q_impl{P}] =====

      Quasi-OCaml, writing parameterised units as functors:

      {[
        module Q_impl (P : P) : Q = struct end        (* argument for Q *)
        module Foo_q (Q : Q) = struct
          let name = "Foo_q"
        end
        module Nested_arg (P : P) = struct
          module Foo_q_of_q_impl = Foo_q (Q_impl (P))
          let describe = "Nested_arg > " ^ Foo_q_of_q_impl.name
        end
      ]}

      [Nested_arg]'s cmi records the compound reference
      [Foo_q[Q:Q_impl{P}]] in its bound_globals: [Foo_q]'s [Q] is
      filled by [Q_impl], whose own [P] is still open.  Functorizing
      [Nested_arg] pulls [Foo_q] and [Q_impl] in transitively and
      specialises the compound reference to the bundle's [P]
      parameter, roughly:

      {[
        module Make (P : P) () = struct
          module DEP__Q_impl = Q_impl (P)
          module DEP__Foo_q = Foo_q (DEP__Q_impl)
          module Nested_arg = (* body of Nested_arg, with
             [Foo_q_of_q_impl := DEP__Foo_q] *)
        end
      ]} *)

   (* Step 1: build [Foo_q], a plain library parameterised by [Q]. *)

   flags = "$flg_int_iface -parameter Q -I q";
   module = "foo_q/foo_q__.ml";
   ocamlc.byte;

   flags = "$flg -parameter Q -I p -I q -H foo_q -open-cmi foo_q/foo_q__.cmi";
   module = "foo_q/foo_q.mli foo_q/foo_q.ml";
   ocamlc.byte;

   (* Step 2: build [Nested_arg] (parameterised by [P]) whose body
      references [Foo_q[Q:Q_impl{P}]] — [Foo_q] applied to the
      parameterised argument [Q_impl].  This lands the compound
      reference in [Nested_arg]'s bound_globals. *)

   flags = "$flg_int_iface -parameter P -I p -I q -I q_impl -I foo_q";
   module = "nested_arg/nested_arg__.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -I p -I q -I q_impl -I foo_q \
     -H nested_arg -open-cmi nested_arg/nested_arg__.cmi";
   module = "nested_arg/nested_arg.mli nested_arg/nested_arg.ml";
   ocamlc.byte;

   (* Step 3: functorize [Nested_arg].  [Foo_q] and [Q_impl] are pulled
      in transitively and the compound reference is specialised in the
      result. *)

   flags = "$flg -functorize -I p -I q -I q_impl -I foo_q -I nested_arg \
     Nested_arg";
   module = "";
   program = "bundle_nested/bundle.cmo";
   all_modules = "";
   ocamlc.byte;

   (* Step 4: consume the result by applying [Bundle.Make (P_int) ()]
      and printing [Inst.Nested_arg.describe] (which composes strings
      from [Nested_arg] and, via the compound alias, [Foo_q]). *)

   flags = "$flg -I bundle_nested -I p -I p_int -I q -I q_impl \
     -I foo_q -I nested_arg";
   module = "main_nested_arg.ml";
   ocamlc.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_functorize_nested_arg.bc";
   all_modules = "\
     foo_q/foo_q__.cmo \
     foo_q/foo_q.cmo \
     q_impl/q_impl__.cmo \
     q_impl/q_impl.cmo \
     nested_arg/nested_arg__.cmo \
     nested_arg/nested_arg.cmo \
     p_int/p_int__.cmo \
     p_int/p_int.cmo \
     bundle_nested/bundle.cmo \
     main_nested_arg.cmo \
   ";
   ocamlc.byte;

   stdout = "test_functorize_nested_arg.output";
   stderr = "test_functorize_nested_arg.output";
   output = "test_functorize_nested_arg.output";
   run;

   reference = "test_functorize_nested_arg.reference";
   check-program-output;
 }{
   (* ===== Case 2: Partial_pq — Pair_pq[Q:Q_impl{P}]{P} =====

      Like case 1, but the applied library also has an UNFILLED
      parameter.  Quasi-OCaml:

      {[
        module Pair_pq (P : P) (Q : Q) = struct
          let name p = "Pair_pq[P=" ^ P.to_string p ^ "]"
        end
        module Partial_pq (P : P) = struct
          module Pair_pq_q_impl = Pair_pq (P) (Q_impl (P))
          let describe p = "Partial_pq > " ^ Pair_pq_q_impl.name p
        end
      ]}

      [Partial_pq]'s cmi records [Pair_pq[Q:Q_impl{P}]{P}]: [Pair_pq]'s
      [Q] is filled visibly (by [Q_impl], whose [P] is open) while
      [Pair_pq]'s own [P] stays a hidden, unfilled argument — a
      partial application in bound_globals.  The bundle's [Make] must
      apply [Pair_pq] to both its [P] parameter and the locally-bound
      [Q_impl(P)]. *)

   (* Step 1: build [Pair_pq], a plain library parameterised by both
      [P] and [Q]. *)

   flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
   module = "pair_pq/pair_pq__.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -parameter Q -I p -I q -H pair_pq \
     -open-cmi pair_pq/pair_pq__.cmi";
   module = "pair_pq/pair_pq.mli pair_pq/pair_pq.ml";
   ocamlc.byte;

   (* Step 2: build [Partial_pq] (parameterised by [P] only) whose body
      references [Pair_pq[Q:Q_impl{P}]{P}] — [Pair_pq]'s [Q] is filled
      by [Q_impl], but its [P] is left unfilled at the top. *)

   flags = "$flg_int_iface -parameter P -I p -I q -I q_impl -I pair_pq";
   module = "partial_pq/partial_pq__.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -I p -I q -I q_impl -I pair_pq -H partial_pq \
     -open-cmi partial_pq/partial_pq__.cmi";
   module = "partial_pq/partial_pq.mli partial_pq/partial_pq.ml";
   ocamlc.byte;

   (* Step 3: functorize [Partial_pq].  [Pair_pq] and [Q_impl] are
      pulled in and the compound reference is specialised. *)

   flags = "$flg -functorize -I p -I q -I q_impl -I pair_pq -I partial_pq \
     Partial_pq";
   module = "";
   program = "bundle_partial/bundle.cmo";
   all_modules = "";
   ocamlc.byte;

   (* Step 4: consume the result by applying [Bundle.Make (P_int) ()]
      and printing [Inst.Partial_pq.describe]. *)

   flags = "$flg -I bundle_partial -I p -I p_int -I q -I q_impl -I pair_pq \
     -I partial_pq";
   module = "main_partial_pq.ml";
   ocamlc.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_functorize_partial_pq.bc";
   all_modules = "\
     pair_pq/pair_pq__.cmo \
     pair_pq/pair_pq.cmo \
     q_impl/q_impl__.cmo \
     q_impl/q_impl.cmo \
     partial_pq/partial_pq__.cmo \
     partial_pq/partial_pq.cmo \
     p_int/p_int__.cmo \
     p_int/p_int.cmo \
     bundle_partial/bundle.cmo \
     main_partial_pq.cmo \
   ";
   ocamlc.byte;

   stdout = "test_functorize_partial_pq.output";
   stderr = "test_functorize_partial_pq.output";
   output = "test_functorize_partial_pq.output";
   run;

   reference = "test_functorize_partial_pq.reference";
   check-program-output;
 }{
   (* ===== Case 3: Nested_r — arg-block extraction =====

      Quasi-OCaml:

      {[
        module R_impl (P : P) = struct
          let filler = #3.5                  (* not part of [R]'s sig *)
          let greeting () = "R_impl greeting, P=" ^ ...
        end
        module Foo_r (R : R) = struct
          let describe () = "Foo_r > " ^ R.greeting ()
        end
        module Nested_r (P : P) (R : R) = struct
          module Foo_r_of_r_impl = Foo_r (R_impl (P) :> R)
          let describe () = Foo_r_of_r_impl.describe ()
        end
      ]}

      [R_impl] is parameterised (by [P]) and its primary block has an
      extra unboxed [filler] field before [greeting], making it a
      MIXED block whose value fields (including the synthesized R-arg
      block) are physically reordered ahead of the flat suffix.  When
      the bundle runs, [R_impl(P)]'s main block must be projected with
      [mod_field] at [arg_block_idx] — using the block's mixed
      representation ([mb_returned_repr]) — to yield [R_impl]'s arg
      block before being passed to [Foo_r]'s functor. *)

   (* Parameter R and argument R_int. *)

   flags = "$flg_int_iface";
   module = "r/r__.ml";
   ocamlc.byte;

   flags = "$flg -as-parameter -H r -open-cmi r/r__.cmi";
   module = "r/r.mli";
   ocamlc.byte;

   flags = "$flg -as-argument-for R -I r -H r_int";
   module = "r_int/r_int.mli r_int/r_int.ml";
   ocamlc.byte;

   (* R_impl: -as-argument-for R -parameter P.  A parameterised argument
      with an extra [filler] field before [greeting]. *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "r_impl/r_impl__.ml";
   ocamlc.byte;

   flags = "$flg -as-argument-for R -parameter P -I p -I r -H r_impl \
     -open-cmi r_impl/r_impl__.cmi";
   module = "r_impl/r_impl.mli r_impl/r_impl.ml";
   ocamlc.byte;

   (* Foo_r: parameterised by R only; calls [R.greeting ()]. *)

   flags = "$flg_int_iface -parameter R -I r";
   module = "foo_r/foo_r__.ml";
   ocamlc.byte;

   flags = "$flg -parameter R -I p -I r -H foo_r \
     -open-cmi foo_r/foo_r__.cmi";
   module = "foo_r/foo_r.mli foo_r/foo_r.ml";
   ocamlc.byte;

   (* Nested_r: parameterised by P and R; uses [Foo_r(R)(R_impl)]. *)

   flags = "$flg_int_iface -parameter P -parameter R -I p -I r -I r_impl \
     -I foo_r";
   module = "nested_r/nested_r__.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -parameter R -I p -I r -I r_impl -I foo_r \
     -H nested_r -open-cmi nested_r/nested_r__.cmi";
   module = "nested_r/nested_r.mli nested_r/nested_r.ml";
   ocamlc.byte;

   (* Bundle [Nested_r]. *)

   flags = "$flg -functorize -I p -I r -I r_impl -I foo_r -I nested_r \
     Nested_r";
   module = "";
   program = "bundle_nested_r/bundle.cmo";
   all_modules = "";
   ocamlc.byte;

   flags = "$flg -I bundle_nested_r -I p -I p_int -I r -I r_int -I r_impl \
     -I foo_r -I nested_r";
   module = "main_nested_r.ml";
   ocamlc.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_functorize_nested_r.bc";
   all_modules = "\
     foo_r/foo_r__.cmo \
     foo_r/foo_r.cmo \
     r_impl/r_impl__.cmo \
     r_impl/r_impl.cmo \
     nested_r/nested_r__.cmo \
     nested_r/nested_r.cmo \
     p_int/p_int__.cmo \
     p_int/p_int.cmo \
     r_int/r_int.cmo \
     bundle_nested_r/bundle.cmo \
     main_nested_r.cmo \
   ";
   ocamlc.byte;

   stdout = "test_functorize_nested_r.output";
   stderr = "test_functorize_nested_r.output";
   output = "test_functorize_nested_r.output";
   run;

   reference = "test_functorize_nested_r.reference";
   check-program-output;
 }
*)
