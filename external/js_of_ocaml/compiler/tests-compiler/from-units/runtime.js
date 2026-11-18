// Stubs for the runtime primitives the test programs call. OCaml passes
// [()] as 0, so scripts call OCaml [unit -> _] functions as [f(0)].
const sharedException = [248, "shared", 17];

// A deferred root's initializer is not run at startup. The program passes it
// to the root's split-anchor primitive, [caml_jsoo_split_anchor__<root>],
// and in production the partition loader calls it later. These stubs stand
// in for the loader: they record each root's initializer so that scenario
// scripts can load the root explicitly with [loadDeferredRoot.<root>(0)].
const loadDeferredRoot = {};

const stubs = {
  // OCaml string constants arrive as [caml_string_of_jsbytes "..."].
  caml_string_of_jsbytes: s => s,

  // Fixtures call these to show which code ran, in what order.
  report_progress: message => { 
    console.log(message); 
    return 0; 
  },
  report_value: (message, value) => { 
    console.log(`${message} [${value}]`); 
    return 0; 
  },
  partition_exception: () => sharedException,
  // A unit whose initializer raises before it stores its module has no definition in
  // the program, so references to it are looked up in the global registry. Nothing
  // here registers units, so the lookup finds nothing.
  caml_get_global: () => undefined,
  caml_wrap_exception: x => x,
  caml_maybe_attach_backtrace: x => x,
  caml_jsoo_split_run: (state, f) => {
    if (state[1]) throw state[2];
    try {
      return f(0);
    } catch (e) {
      state[1] = 1;
      state[2] = e;
      throw e;
    }
  },
  caml_make_vect: (length, value) => [0, ...new Array(length).fill(value)],
  caml_array_unsafe_get: (array, i) => array[i + 1],
  caml_array_unsafe_set: (array, i, value) => { array[i + 1] = value; return 0; },
  caml_int64_create_lo_mi_hi: (lo, mi, hi) => ({lo, mi, hi: hi << 16}),
  partition_float_negative: x => {
    if (typeof x !== "number") throw new Error("expected float");
    return +(x < 0);
  },
  partition_int64_negative: x => {
    if (typeof x !== "object" || typeof x.hi !== "number")
      throw new Error("expected int64");
    return +(x.hi < 0);
  }
};

// Adds primitives that only one scenario's program calls. Call it before loading
// that program.
function addPrimitives(primitives) {
  Object.assign(stubs, primitives);
}

const anchor = "caml_jsoo_split_anchor__";

globalThis.jsoo_runtime = new Proxy(stubs, {get(target, name) {
  if (typeof name === "string" && name.startsWith(anchor))
    return initializer => {
      loadDeferredRoot[name.slice(anchor.length)] = initializer;
      return 0;
    };
  return target[name];
}});
