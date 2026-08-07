(* TEST (* DO NOT EDIT. Instead edit instance_arg/test_byte.ml and run gen-native.sh. *)
 (* [User] (a member of a [-parameter P] library) references the complete
    instance [Lib_q[Q:Q_impl]] through a pure alias in its prelude
    [user__.ml].  Under [-no-alias-deps] the prelude's cmi records the
    reference approximately, with the arg value over-approximated as
    [Q_impl{P}].  The functorizer must complete the value against
    q_impl.cmi (no parameters), recognise [Lib_q[Q:Q_impl]] as static,
    and leave it as a global reference instead of bundling it. *)

 readonly_files = "\
   lib_q.mli lib_q.ml \
   user.mli user.ml user__.ml \
   main_instance_arg.ml test_instance_arg.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int q q_impl lib_q user instances bundle";
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

 src = "lib_q.mli lib_q.ml";       dst = "lib_q/";  copy;
 src = "user.mli user.ml user__.ml"; dst = "user/"; copy;

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Step 1: parameters [P] and [Q]; arguments [P_int] and [Q_impl]. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -H p -open-cmi p/p__.cmi";
 module = "p/p.mli";
 ocamlopt.byte;

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for P -I p -H p_int -open-cmi p_int/p_int__.cmi";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlopt.byte;

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -H q -open-cmi q/q__.cmi";
 module = "q/q.mli";
 ocamlopt.byte;

 flags = "$flg_int_iface";
 module = "q_impl/q_impl__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for Q -I q -H q_impl \
   -open-cmi q_impl/q_impl__.cmi";
 module = "q_impl/q_impl.ml";
 ocamlopt.byte;

 (* Step 2: build [Lib_q], parameterised by [Q]. *)

 flags = "$flg -parameter Q -I q -I lib_q";
 module = "lib_q/lib_q.mli lib_q/lib_q.ml";
 ocamlopt.byte;

 (* Step 3: pre-instantiate [Lib_q[Q:Q_impl]] into [instances/]. *)

 flags = "$flg -I q -I q_impl -I lib_q -instantiate";
 module = "";
 program = "instances/lib_q-Q_impl.cmx";
 all_modules = "lib_q/lib_q.cmx q_impl/q_impl.cmx";
 ocamlopt.byte;

 (* Step 4: build [User] (parameterised by [P]) whose prelude
    pure-aliases [Lib_q(Q)(Q_impl)] and whose body uses the alias. *)

 flags = "$flg_int_iface -parameter P -I q -I q_impl -I lib_q -I instances";
 module = "user/user__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I q -I q_impl -I lib_q -I instances \
   -H user -open-cmi user/user__.cmi";
 module = "user/user.mli user/user.ml";
 ocamlopt.byte;

 (* Step 5: functorize [User].  [User__]'s approximate reference
    [Lib_q[Q:Q_impl{P}]] is completed to the static [Lib_q[Q:Q_impl]]
    and left as a global reference. *)

 flags = "$flg -functorize -I p -I q -I q_impl -I lib_q -I instances \
   -I user User";
 module = "";
 program = "bundle/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* Step 6: consume the bundle. *)

 flags = "$flg -I bundle -I p -I p_int -I q -I q_impl -I lib_q \
   -I instances -I user";
 module = "main_instance_arg.ml";
 ocamlopt.byte;

 flags = "$flg_link";
 module = "";
 program = "$test_build_directory/test_instance_arg.exe";
 all_modules = "\
   q_impl/q_impl__.cmx \
   q_impl/q_impl.cmx \
   lib_q/lib_q.cmx \
   instances/lib_q-Q_impl.cmx \
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   user/user__.cmx \
   user/user.cmx \
   bundle/bundle.cmx \
   main_instance_arg.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_instance_arg.output";
 stderr = "test_instance_arg.output";
 output = "test_instance_arg.output";
 run;

 reference = "test_instance_arg.reference";
 check-program-output;
*)
