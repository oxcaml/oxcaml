Joint parsing, deferred initializers, and the pre-lowering optimization boundary.
`./driver.exe parse` parses its arguments as one program and then runs each
action flag in order; see `./driver.exe parse -help`. The long
array_discriminators.ml fixture, the runtime.js primitive stubs and the
js_server.js runner live next to this test.

  $ if [ ! -f array_discriminators.ml ]; then
  >   for file in driver.exe array_discriminators.ml runtime.js js_server.js; do
  >     cp "$TESTDIR/$file" .
  >   done
  > fi
  $ OCAMLC=${OCAMLC:-ocamlc}
  $ NODE=${NODE:-node}
  $ compile() {
  >   for source in "$@"; do
  >     $OCAMLC -g -c "$source" || return 1
  >   done
  > }

Generated programs run in one long-lived Node process per scenario, so each
step can be its own command, with its output right below it:

- start_js starts the process, which loads the stubs in runtime.js;
- js CODE runs CODE and prints its output;
- load_js FILE runs a whole file, usually a generated program;
- stop_js ends the process.

Every step shares one global context, so a later step sees the stubs, the
program's state and the roots it registered. A step can add primitives that only
its scenario needs with addPrimitives({name: implementation, ...}), before
loading the program that calls them. The program hands each deferred root's
initializer to the runtime instead of running it; loadDeferredRoot.ID(0) runs
root ID. A step that raises Shared.exn prints "Raised Shared.exn"; any other
exception is printed as unexpected.

  $ start_js() {
  >   mkfifo js.in js.out
  >   "$NODE" js_server.js < js.in > js.out 2>&1 &
  >   exec 3> js.in 4< js.out
  > }
  $ js() {
  >   printf '%s\0' "$1" >&3
  >   while IFS= read -r line <&4; do
  >     [ "$line" = __js_done__ ] && break
  >     printf '%s\n' "$line"
  >   done
  > }
  $ load_js() { js "$(cat "$1")"; }
  $ stop_js() {
  >   exec 3>&- 4<&-
  >   wait
  >   rm js.in js.out
  > }

Export selection changes registrations, not imports or initializer effects.
Without -exported-unit, every unit is registered.

  $ cat > exports_a.ml <<'EOF'
  > let f x = x + 1
  > EOF
  $ cat > exports_b.ml <<'EOF'
  > let x = Exports_a.f 41
  > let fail () = assert false
  > let fail_again () = assert false
  > EOF
  $ compile exports_a.ml exports_b.ml
  $ ./driver.exe parse exports_a.cmo exports_b.cmo -print-registry-calls
  caml_get_global_predef Assert_failure
  caml_register_global Exports_a
  caml_register_global Exports_b
  $ ./driver.exe parse -exported-unit Exports_a \
  >   exports_a.cmo exports_b.cmo \
  >   -print-registry-calls
  caml_get_global_predef Assert_failure
  caml_register_global Exports_a

With no exported units, Exports_b still imports Exports_a through the registry.

  $ ./driver.exe parse -no-exports exports_b.cmo -print-registry-calls
  caml_get_global Exports_a
  caml_get_global_predef Assert_failure

-include-cmis lists the CMIs a toplevel embeds. It covers every parsed unit,
including units that are not exported.

  $ ./driver.exe parse -include-cmis -exported-unit Exports_a \
  >   exports_a.cmo exports_b.cmo \
  >   -print-cmis
  Exports_a
  Exports_b

Parsing units jointly produces the same IR as parsing them from an archive.

  $ cat > optimization_a.ml <<'EOF'
  > external report_progress : string -> unit = "report_progress"
  > external report_value : string -> int -> unit = "report_value"
  > let f x = x + 1
  > let dead () = report_progress "Optimization_a.dead: unused"
  > let () = report_progress "Optimization_a: initialized"
  > EOF
  $ cat > optimization_b.ml <<'EOF'
  > let () =
  >   Optimization_a.report_value
  >     "Optimization_b: Optimization_a.f 41 returned"
  >     (Optimization_a.f 41)
  > EOF
  $ compile optimization_a.ml optimization_b.ml
  $ $OCAMLC -a optimization_a.cmo optimization_b.cmo -o optimization.cma
  $ ./driver.exe parse optimization_a.cmo optimization_b.cmo \
  >   -print-ir > units.ir
  $ ./driver.exe parse optimization.cma -print-ir > archive.ir
  $ diff -u units.ir archive.ir

O3 inlines across unit boundaries. It keeps an exported function, which other
code may look up, but removes it when nothing is exported. Initialization
effects are retained either way.

[Optimization_a.dead] is the only code with the message
"Optimization_a.dead: unused", so this counts whether it survives when
[Optimization_a] is exported:

  $ ./driver.exe parse optimization_a.cmo optimization_b.cmo \
  >   -emit-js exported.js
  $ grep -c -F '"Optimization_a.dead: unused"' exported.js
  1
  $ ./driver.exe parse -no-exports \
  >   optimization_a.cmo optimization_b.cmo \
  >   -emit-js optimized.js
  $ cat optimized.js
  (function(globalThis){
     "use strict";
     var
      runtime = globalThis.jsoo_runtime,
      caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
     runtime.report_progress
      (caml_string_of_jsbytes("Optimization_a: initialized"));
     runtime.report_value
      (caml_string_of_jsbytes("Optimization_b: Optimization_a.f 41 returned"),
       42);
     return;
    }
    (globalThis));
  $ start_js
  $ load_js optimized.js
  Optimization_a: initialized
  Optimization_b: Optimization_a.f 41 returned [42]
  $ stop_js


Debug locations and inlining hints are the same whether units are parsed
jointly or separately.

  $ cat > metadata_a.ml <<'EOF'
  > let[@inline never] f x = x + 1
  > EOF
  $ cat > metadata_b.ml <<'EOF'
  > (* Keep this function on a different line from metadata_a.ml's function. *)
  > let[@inline always] g x = x + 2
  > EOF
  $ compile metadata_a.ml metadata_b.ml
  $ ./driver.exe parse -debug \
  >   metadata_a.cmo metadata_b.cmo \
  >   -print-closures | tee metadata-joint.txt
  metadata_a.ml:1 no hint
  metadata_b.ml:2 no hint
  $ { ./driver.exe parse -debug metadata_a.cmo -print-closures
  >   ./driver.exe parse -debug metadata_b.cmo -print-closures
  > } | LC_ALL=C sort | diff -u metadata-joint.txt -

An empty eager program is valid.

  $ ./driver.exe parse -no-exports \
  >   -print-registry-calls -print-cmis -print-debug-info -emit-js empty.js
  Debug info: [empty]
  $ cat empty.js
  (function(globalThis){"use strict"; return;}(globalThis));

Deferred initialization runs only when its root is loaded, observes eager
state live rather than a copy of it, and keeps exception identity: Shared.exn
raised by the root is the object eager code created, both inside the root and
when it escapes to the loader. -root starts a deferred root; the units after it
belong to that root.

Shared is the eager unit the roots depend on. Its [shared] reference is the
state they share, and Expose hands [Shared.set] and [Shared.get] to JavaScript
through a primitive this scenario adds to the runtime itself:

  $ cat > shared.ml <<'EOF'
  > external report_progress : string -> unit = "report_progress"
  > external report_value : string -> int -> unit = "report_value"
  > external make_exception : unit -> exn = "partition_exception"
  > let exn = make_exception ()
  > let shared = ref 1
  > let set n = shared := n
  > let get () = !shared
  > let () = report_progress "Shared: initialized"
  > EOF
  $ cat > expose.ml <<'EOF'
  > external expose : (int -> unit) -> (unit -> int) -> unit = "partition_expose"
  > let () = expose Shared.set Shared.get
  > EOF
  $ expose_shared() {
  >   js 'addPrimitives({
  >     partition_expose: (set, get) => {
  >       globalThis.Shared = {set, get};
  >       return 0;
  >     }
  >   })'
  > }

The program has two roots. [state] reports the value of Shared.shared it sees,
catches Shared.exn inside a loop, and increments Shared.shared. [exception] sets
Shared.shared to 100, then raises Shared.exn out of itself:

  $ cat > deferred_state.ml <<'EOF'
  > let dead () = Shared.report_progress "Deferred_state.dead: unused"
  > let () =
  >   Shared.report_value "Deferred_state: Shared.shared is" !Shared.shared;
  >   (try
  >      for i = 1 to 3 do
  >        if i = 3 then raise Shared.exn;
  >        Shared.report_value "Deferred_state: loop iteration" i
  >      done
  >    with
  >    | exn ->
  >      Shared.report_progress
  >        (if exn == Shared.exn
  >         then "Deferred_state: caught Shared.exn"
  >         else "Deferred_state: caught a different exception"));
  >   incr Shared.shared
  > EOF
  $ cat > deferred_exception.ml <<'EOF'
  > let () =
  >   Shared.shared := 100;
  >   Shared.report_progress "Deferred_exception: raising Shared.exn";
  >   raise Shared.exn
  > EOF
  $ compile shared.ml expose.ml deferred_state.ml deferred_exception.ml
  $ ./driver.exe parse -invariants -no-exports \
  >   shared.cmo expose.cmo \
  >   -root state deferred_state.cmo \
  >   -root exception deferred_exception.cmo \
  >   -emit-js shared-state.js

Dead code in a deferred root is removed too. [Deferred_state.dead] is the only
code with the message "Deferred_state.dead: unused":

  $ grep -c -F '"Deferred_state.dead: unused"' shared-state.js
  0
  [1]

Startup initializes Shared, but neither root reports anything:

  $ start_js
  $ expose_shared
  $ load_js shared-state.js
  Shared: initialized

Set Shared.shared to 7, then load [state]. It sees 7, not the initial 1, so it
ran only when loaded. The exception it catches is Shared.exn itself:

  $ js 'Shared.set(7)'
  $ js 'loadDeferredRoot.state(0)'
  Deferred_state: Shared.shared is [7]
  Deferred_state: loop iteration [1]
  Deferred_state: loop iteration [2]
  Deferred_state: caught Shared.exn

The root's increment is visible to eager code, so both use the same reference
rather than copies:

  $ js 'console.log(`Shared.shared is [${Shared.get(0)}]`)'
  Shared.shared is [8]

Load [exception] separately. The Shared.exn it raises reaches the loader as the
same object eager code created, and its effect from before the raise remains:

  $ js 'loadDeferredRoot.exception(0)'
  Deferred_exception: raising Shared.exn
  Raised Shared.exn
  $ js 'console.log(`Shared.shared is [${Shared.get(0)}]`)'
  Shared.shared is [100]
  $ stop_js

The eager part may be empty

  $ cat > deferred_reporter.ml <<'EOF'
  > external report_progress : string -> unit = "report_progress"
  > let () = report_progress "Deferred_reporter: initialized"
  > EOF
  $ compile deferred_reporter.ml
  $ ./driver.exe parse -invariants -no-exports \
  >   -root deferred_reporter deferred_reporter.cmo \
  >   -emit-js empty-eager.js
  $ start_js
  $ load_js empty-eager.js
  $ js 'loadDeferredRoot.deferred_reporter(0)'
  Deferred_reporter: initialized
  $ stop_js

But a declared deferred root must contain bytecode. An archive with no units is a
valid file whose bytecode is empty:

  $ $OCAMLC -a -o empty.cma
  $ ./driver.exe parse -invariants -no-exports -root lazy empty.cma
  Deferred_code.create: empty deferred code is not allowed
  [1]

Optimizing for Wasm doesn't inline a deferred root's initializer away: it stays
a separate one-argument function, passed to the root's split anchor.

  $ cat > wasm_deferred.ml <<'EOF'
  > external report_value : string -> int -> unit = "report_value"
  > let () = report_value "Wasm_deferred: Exports_a.f 41 returned" (Exports_a.f 41)
  > EOF
  $ compile wasm_deferred.ml
  $ ./driver.exe parse -target wasm -invariants -no-exports \
  >   exports_a.cmo \
  >   -root lazy wasm_deferred.cmo \
  >   -optimize-for-wasm -print-split-anchors
  Root [lazy] initializer arity: [1]

Deferred roots run as if they were the rest of the eager code, in the order
they are loaded. An exception that escapes a root ends the program, as it would
in eager code: no deferred code runs afterwards, and loading another root raises
that exception again.

  $ cat > sibling_a.ml <<'EOF'
  > let () =
  >   Shared.report_progress "Sibling_a: raising";
  >   raise Shared.exn
  > EOF
  $ cat > sibling_b.ml <<'EOF'
  > let () = Shared.report_progress "Sibling_b: initialized"
  > EOF
  $ cat > sibling_c.ml <<'EOF'
  > let () =
  >   for i = 1 to 2 do
  >     Shared.report_value "Sibling_c: loop iteration" i
  >   done
  > EOF
  $ compile sibling_a.ml sibling_b.ml sibling_c.ml
  $ ./driver.exe parse -invariants -no-exports \
  >   shared.cmo \
  >   -root A sibling_a.cmo \
  >   -root B sibling_b.cmo \
  >   -root C sibling_c.cmo \
  >   -check-invariant -optimize-for-partition -print-split-anchors \
  >   -emit-js siblings-abc.js
  IR invariants: [valid]
  Root [A] initializer arity: [1]
  Root [B] initializer arity: [1]
  Root [C] initializer arity: [1]

Loading C, A, then B behaves like eager code running C, then A: A raises, so B
never runs, and loading it raises A's exception again. Each root runs at most
once, so loading C a second time does nothing:

  $ start_js
  $ load_js siblings-abc.js
  Shared: initialized
  $ js 'loadDeferredRoot.C(0)'
  Sibling_c: loop iteration [1]
  Sibling_c: loop iteration [2]
  $ js 'loadDeferredRoot.C(0)'
  $ js 'loadDeferredRoot.A(0)'
  Sibling_a: raising
  Raised Shared.exn
  $ js 'loadDeferredRoot.B(0)'
  Raised Shared.exn
  $ stop_js

Only the load order matters, not the order of the roots in the program:
  $ ./driver.exe parse -invariants -no-exports \
  >   shared.cmo \
  >   -root C sibling_c.cmo \
  >   -root B sibling_b.cmo \
  >   -root A sibling_a.cmo \
  >   -check-invariant -optimize-for-partition -print-split-anchors \
  >   -emit-js siblings-cba.js
  IR invariants: [valid]
  Root [A] initializer arity: [1]
  Root [B] initializer arity: [1]
  Root [C] initializer arity: [1]
  $ start_js
  $ load_js siblings-cba.js
  Shared: initialized
  $ js 'loadDeferredRoot.C(0)'
  Sibling_c: loop iteration [1]
  Sibling_c: loop iteration [2]
  $ js 'loadDeferredRoot.A(0)'
  Sibling_a: raising
  Raised Shared.exn
  $ js 'loadDeferredRoot.B(0)'
  Raised Shared.exn
  $ stop_js

Partition optimization stops before trampoline lowering.

  $ cat > trampolines.ml <<'EOF'
  > let rec odd x = if x = 0 then false else even (x - 1)
  > and even x = if x = 0 then true else odd (x - 1)
  > EOF
  $ compile trampolines.ml
  $ ./driver.exe parse trampolines.cmo \
  >   -optimize-for-partition -print-trampolines
  Has trampolines: [false]
  $ ./driver.exe parse trampolines.cmo \
  >   -optimize-for-wasm -print-trampolines
  Has trampolines: [true]

Adding the pre-lowering optimization pass preserves deferred state semantics.

Counter is a reference, which it hands to JavaScript through its own primitive:

  $ cat > counter.ml <<'EOF'
  > external report_value : string -> int -> unit = "report_value"
  > external expose : (int -> unit) -> (unit -> int) -> unit = "counter_expose"
  > let value = ref 1
  > let () = expose (fun n -> value := n) (fun () -> !value)
  > EOF
  $ expose_counter() {
  >   js 'addPrimitives({
  >     counter_expose: (set, get) => {
  >       globalThis.Counter = {set, get};
  >       return 0;
  >     }
  >   })'
  > }
  $ cat > staged.ml <<'EOF'
  > let () =
  >   Counter.report_value "Staged: Counter.value is" !Counter.value;
  >   incr Counter.value
  > EOF
  $ compile counter.ml staged.ml
  $ ./driver.exe parse -invariants -no-exports \
  >   counter.cmo \
  >   -root state staged.cmo \
  >   -emit-js state-ordinary.js
  $ ./driver.exe parse -invariants -no-exports \
  >   counter.cmo \
  >   -root state staged.cmo \
  >   -optimize-for-partition -emit-js state-staged.js

Run the same steps against both programs and compare:

  $ shared_state_steps() {
  >   start_js
  >   expose_counter
  >   load_js "$1"
  >   js 'Counter.set(7)'
  >   js 'loadDeferredRoot.state(0)'
  >   js 'console.log(`Counter.value is [${Counter.get(0)}]`)'
  >   stop_js
  > }
  $ shared_state_steps state-ordinary.js > state-ordinary.out
  $ shared_state_steps state-staged.js > state-staged.out
  $ diff -u state-ordinary.out state-staged.out
  $ cat state-staged.out
  Staged: Counter.value is [7]
  Counter.value is [8]

The same optimization pass keeps float and non-float arrays distinct when only
a GADT witness, not the static type, says which one an array is. A flat float
array read as boxed values (or vice versa) would give the wrong sign:

  $ compile array_discriminators.ml
  $ ./driver.exe parse -no-exports \
  >   array_discriminators.cmo \
  >   -emit-js arrays-ordinary.js
  $ ./driver.exe parse -no-exports \
  >   array_discriminators.cmo \
  >   -optimize-for-partition -emit-js arrays-staged.js
  $ { start_js; load_js arrays-ordinary.js; stop_js; } > arrays-ordinary.out
  $ { start_js; load_js arrays-staged.js; stop_js; } > arrays-staged.out
  $ diff -u arrays-ordinary.out arrays-staged.out
  $ cat arrays-staged.out
  float bucket is not negative
  span bucket is negative

Unconditional raises preserve IR invariants, both while parsing and after O3.
An eager raise must still prevent startup from reaching any deferred roots.

  $ cat > raising.ml <<'EOF'
  > external make_exception : unit -> exn = "partition_exception"
  > let () = raise (make_exception ())
  > EOF
  $ cat > raising_eager.ml <<'EOF'
  > external make_exception : unit -> exn = "partition_exception"
  > let x = 7
  > let () = raise (make_exception ())
  > EOF
  $ cat > after_raise.ml <<'EOF'
  > external report_value : string -> int -> unit = "report_value"
  > let () = report_value "After_raise: Raising_eager.x is" Raising_eager.x
  > EOF
  $ compile raising.ml raising_eager.ml after_raise.ml

A deferred root that raises:

  $ ./driver.exe parse -invariants -no-exports \
  >   -root A raising.cmo \
  >   -check-invariant
  IR invariants: [valid]
  $ ./driver.exe parse -no-exports \
  >   -root A raising.cmo \
  >   -optimize-for-partition -check-invariant -emit-js sole-deferred-raise.js
  IR invariants: [valid]
  $ start_js
  $ load_js sole-deferred-raise.js
  $ js 'loadDeferredRoot.A(0)'
  Raised Shared.exn
  $ stop_js

The same units parsed eagerly, for comparison:

  $ ./driver.exe parse raising_eager.cmo after_raise.cmo \
  >   -emit-js eager-raise-baseline.js
  $ start_js
  $ load_js eager-raise-baseline.js
  Raised Shared.exn

No deferred root was passed, so there should be nothing here

  $ js 'console.log(`Registered roots: [${Object.keys(loadDeferredRoot)}]`)'
  Registered roots: []
  $ stop_js

An eager raise before one deferred root:

  $ ./driver.exe parse -invariants -no-exports \
  >   raising_eager.cmo \
  >   -root B after_raise.cmo \
  >   -check-invariant
  IR invariants: [valid]
  $ ./driver.exe parse -no-exports \
  >   raising_eager.cmo \
  >   -root B after_raise.cmo \
  >   -optimize-for-partition -check-invariant -emit-js eager-raise-one-root.js
  IR invariants: [valid]
  $ start_js
  $ load_js eager-raise-one-root.js
  Raised Shared.exn

Deferred roots are registered in the eager code, so they should not be here if 
eager raised immediately

  $ js 'console.log(`Registered roots: [${Object.keys(loadDeferredRoot)}]`)'
  Registered roots: []
  $ stop_js

An eager raise before sibling roots:

  $ ./driver.exe parse -invariants -no-exports \
  >   raising_eager.cmo \
  >   -root A raising.cmo \
  >   -root B after_raise.cmo \
  >   -check-invariant
  IR invariants: [valid]
  $ ./driver.exe parse -no-exports \
  >   raising_eager.cmo \
  >   -root A raising.cmo \
  >   -root B after_raise.cmo \
  >   -optimize-for-partition -check-invariant \
  >   -emit-js eager-raise-sibling-roots.js
  IR invariants: [valid]
  $ start_js
  $ load_js eager-raise-sibling-roots.js
  Raised Shared.exn

The raise happens before startup registers either root, so neither is here

  $ js 'console.log(`Registered roots: [${Object.keys(loadDeferredRoot)}]`)'
  Registered roots: []
  $ stop_js

# CR-soon amin: Update this test to use the [-dependency] syntax, so the compiler
# infers where [Shared_by_deferreds], [Shared_with_eager], [Only_in_a], and 
# [Eager_only] go instead of the test placing them by hand.

Each initializer runs exactly once, wherever its side effect is needed:

1. Shared_by_deferreds is used by both roots but not by eager code.
2. Shared_with_eager is used by eager code (Main) and by root A.
3. Eager_only is used only by eager code.
4. Only_in_a belongs to root A alone.

A unit used by more than one root, or by eager code, must be eager, so 1-3
initialize once at startup, before any root is loaded. Only_in_a initializes
once, when A is loaded. Each unit counts its initializations, and every user
reports the count it sees.

  $ cat > shared_by_deferreds.ml <<'EOF'
  > external report_progress : string -> unit = "report_progress"
  > let initializations = ref 0
  > let () =
  >   incr initializations;
  >   report_progress "Shared_by_deferreds: initialized"
  > EOF
  $ cat > shared_with_eager.ml <<'EOF'
  > external report_progress : string -> unit = "report_progress"
  > let initializations = ref 0
  > let () =
  >   incr initializations;
  >   report_progress "Shared_with_eager: initialized"
  > EOF
  $ cat > eager_only.ml <<'EOF'
  > external report_progress : string -> unit = "report_progress"
  > let initializations = ref 0
  > let () =
  >   incr initializations;
  >   report_progress "Eager_only: initialized"
  > EOF
  $ cat > only_in_a.ml <<'EOF'
  > external report_progress : string -> unit = "report_progress"
  > let initializations = ref 0
  > let () =
  >   incr initializations;
  >   report_progress "Only_in_a: initialized"
  > EOF
  $ cat > main.ml <<'EOF'
  > external report_value : string -> int -> unit = "report_value"
  > let () =
  >   report_value
  >     "Main: Eager_only initializations"
  >     !Eager_only.initializations;
  >   report_value
  >     "Main: Shared_with_eager initializations"
  >     !Shared_with_eager.initializations
  > EOF
  $ cat > deferred_a.ml <<'EOF'
  > external report_value : string -> int -> unit = "report_value"
  > let () =
  >   report_value
  >     "Deferred_a: Shared_by_deferreds initializations"
  >     !Shared_by_deferreds.initializations;
  >   report_value
  >     "Deferred_a: Shared_with_eager initializations"
  >     !Shared_with_eager.initializations;
  >   report_value
  >     "Deferred_a: Only_in_a initializations"
  >     !Only_in_a.initializations
  > EOF
  $ cat > deferred_b.ml <<'EOF'
  > external report_value : string -> int -> unit = "report_value"
  > let () =
  >   report_value
  >     "Deferred_b: Shared_by_deferreds initializations"
  >     !Shared_by_deferreds.initializations
  > EOF
  $ compile shared_by_deferreds.ml shared_with_eager.ml eager_only.ml only_in_a.ml \
  >   main.ml deferred_a.ml deferred_b.ml
  $ ./driver.exe parse -invariants -no-exports \
  >   shared_by_deferreds.cmo shared_with_eager.cmo eager_only.cmo main.cmo \
  >   -root A only_in_a.cmo deferred_a.cmo \
  >   -root B deferred_b.cmo \
  >   -optimize-for-partition -emit-js once.js

Startup initializes Shared_by_deferreds, Shared_with_eager and Eager_only, but
nothing from either root:

  $ start_js
  $ load_js once.js
  Shared_by_deferreds: initialized
  Shared_with_eager: initialized
  Eager_only: initialized
  Main: Eager_only initializations [1]
  Main: Shared_with_eager initializations [1]

Loading B, then A, initializes only what each root owns:

  $ js 'loadDeferredRoot.B(0)'
  Deferred_b: Shared_by_deferreds initializations [1]
  $ js 'loadDeferredRoot.A(0)'
  Only_in_a: initialized
  Deferred_a: Shared_by_deferreds initializations [1]
  Deferred_a: Shared_with_eager initializations [1]
  Deferred_a: Only_in_a initializations [1]
  $ stop_js

A unit can't appear in more than one place: initializing it twice would define
its globals twice.

  $ ./driver.exe parse -no-exports \
  >   shared_with_eager.cmo eager_only.cmo main.cmo \
  >   -root A shared_by_deferreds.cmo only_in_a.cmo deferred_a.cmo \
  >   -root B shared_by_deferreds.cmo deferred_b.cmo
  Parse_bytecode.from_units: compilation unit Shared_by_deferreds appears more than once
  [1]

A root can't use a value defined by a sibling root. Each root becomes its own
closure, so the value would be out of scope.

  $ ./driver.exe parse -no-exports \
  >   shared_with_eager.cmo eager_only.cmo main.cmo \
  >   -root A shared_by_deferreds.cmo only_in_a.cmo deferred_a.cmo \
  >   -root B deferred_b.cmo
  Parse_bytecode.from_units: deferred root "B" uses a value defined by deferred root "A"
  [1]
