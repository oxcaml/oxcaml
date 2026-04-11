(() => {
  const global = globalThis;
  const bitsBuffer = new ArrayBuffer(4);
  const bitsI32 = new Int32Array(bitsBuffer);
  const bitsF32 = new Float32Array(bitsBuffer);
  function noteShimCall(_name) {}

  function bitsToFloat32(bits) {
    bitsI32[0] = bits | 0;
    return bitsF32[0];
  }

  function float32ToBits(value) {
    bitsF32[0] = Math.fround(value);
    return bitsI32[0] | 0;
  }

  function compareFloat32Bits(leftBits, rightBits) {
    const left = bitsToFloat32(leftBits);
    const right = bitsToFloat32(rightBits);
    return (
      (left > right ? 1 : 0) -
      (left < right ? 1 : 0) +
      (left === left ? 1 : 0) -
      (right === right ? 1 : 0)
    );
  }

  function popcnt32(value) {
    let x = value >>> 0;
    x -= (x >>> 1) & 0x55555555;
    x = (x & 0x33333333) + ((x >>> 2) & 0x33333333);
    x = (x + (x >>> 4)) & 0x0f0f0f0f;
    return ((x * 0x01010101) >>> 24) | 0;
  }

  global.caml_is_boot_compiler = function caml_is_boot_compiler() {
    return 0;
  };

  global.caml_ml_domain_index = function caml_ml_domain_index() {
    noteShimCall("caml_ml_domain_index");
    return 0;
  };

  global.caml_sys_const_arch_amd64 = function caml_sys_const_arch_amd64() {
    noteShimCall("caml_sys_const_arch_amd64");
    return 0;
  };

  global.caml_sys_const_arch_arm64 = function caml_sys_const_arch_arm64() {
    noteShimCall("caml_sys_const_arch_arm64");
    return 1;
  };

  global.caml_eventlog_pause = function caml_eventlog_pause() {
    noteShimCall("caml_eventlog_pause");
    return 0;
  };

  global.caml_eventlog_resume = function caml_eventlog_resume() {
    noteShimCall("caml_eventlog_resume");
    return 0;
  };

  global.caml_sys_io_buffer_size = function caml_sys_io_buffer_size() {
    noteShimCall("caml_sys_io_buffer_size");
    return 65536;
  };

  global.caml_sys_getenv_opt = function caml_sys_getenv_opt(name) {
    noteShimCall(`caml_sys_getenv_opt:${String(name)}`);
    if (
      typeof global.jsoo_sys_getenv !== "function"
    ) {
      return 0;
    }
    const key = String(name);
    const value = global.jsoo_sys_getenv(key);
    if (value === undefined) {
      return 0;
    }
    if (typeof global.caml_string_of_jsbytes === "function") {
      return [0, global.caml_string_of_jsbytes(value)];
    }
    if (typeof global.caml_string_of_jsstring === "function") {
      return [0, global.caml_string_of_jsstring(value)];
    }
    return [0, value];
  };

  global.__oxcaml_domain_tls = [];

  global.caml_domain_tls_get = function caml_domain_tls_get() {
    noteShimCall("caml_domain_tls_get");
    return global.__oxcaml_domain_tls;
  };

  global.caml_domain_tls_set = function caml_domain_tls_set(state) {
    noteShimCall(`caml_domain_tls_set:${state === undefined ? "undefined" : typeof state}`);
    global.__oxcaml_domain_tls = state === undefined ? [] : state;
    return 0;
  };

  global.caml_effective_tick_interval_usec_bytecode =
    function caml_effective_tick_interval_usec_bytecode() {
      noteShimCall("caml_effective_tick_interval_usec_bytecode");
      return 0;
    };

  global.caml_domain_set_tick_interval_usec_bytecode =
    function caml_domain_set_tick_interval_usec_bytecode() {
      noteShimCall("caml_domain_set_tick_interval_usec_bytecode");
      return 0;
    };

  global.caml_gc_tweak_get = function caml_gc_tweak_get() {
    noteShimCall("caml_gc_tweak_get");
    return 0;
  };

  global.caml_gc_tweak_set = function caml_gc_tweak_set() {
    noteShimCall("caml_gc_tweak_set");
    return 0;
  };

  global.caml_gc_tweak_list_active = function caml_gc_tweak_list_active() {
    noteShimCall("caml_gc_tweak_list_active");
    return 0;
  };

  global.caml_memprof_enlist = function caml_memprof_enlist() {
    noteShimCall("caml_memprof_enlist");
    return 0;
  };

  global.caml_memprof_enlist_all_domains =
    function caml_memprof_enlist_all_domains() {
      noteShimCall("caml_memprof_enlist_all_domains");
      return 0;
    };

  global.caml_continuation_update_handler_noexc =
    function caml_continuation_update_handler_noexc(cont, hval, hexn, heff) {
      noteShimCall("caml_continuation_update_handler_noexc");
      if (typeof global.caml_continuation_use_and_update_handler_noexc !== "function") {
        return cont;
      }
      const stack =
        global.caml_continuation_use_and_update_handler_noexc(cont, hval, hexn, heff);
      if (stack !== 0) {
        cont[1] = stack;
      }
      return cont;
    };

  global.caml_float32_of_string = function caml_float32_of_string(source) {
    return Math.fround(global.caml_float_of_string(source));
  };

  global.caml_int_popcnt = function caml_int_popcnt(value) {
    return popcnt32(value);
  };

  global.caml_int32_popcnt = function caml_int32_popcnt(value) {
    return popcnt32(value);
  };

  global.caml_nativeint_popcnt = function caml_nativeint_popcnt(value) {
    return popcnt32(value);
  };

  global.caml_int64_popcnt = function caml_int64_popcnt(value) {
    if (
      typeof global.caml_int64_lo32 === "function" &&
      typeof global.caml_int64_hi32 === "function"
    ) {
      return (popcnt32(global.caml_int64_lo32(value)) + popcnt32(global.caml_int64_hi32(value))) | 0;
    }
    return popcnt32(value);
  };

  global.caml_format_float32 = function caml_format_float32(format, value) {
    return global.caml_format_float(format, value);
  };

  global.caml_int_as_pointer = function caml_int_as_pointer(value) {
    if ((value | 0) === 0) {
      return null;
    }
    return { caml_int_as_pointer: value | 0 };
  };

  global.caml_method_cache = global.caml_method_cache || [];

  global.caml_oo_cache_id = function caml_oo_cache_id() {
    const cacheid = global.caml_method_cache.length;
    global.caml_method_cache[cacheid] = 0;
    return cacheid;
  };

  global.caml_get_cached_method = function caml_get_cached_method(
    obj,
    tag,
    cacheid,
  ) {
    const meths = obj[1];
    const ofs = global.caml_method_cache[cacheid] | 0;
    if (meths[ofs + 4] === tag) {
      return meths[ofs + 3];
    }
    let li = 3;
    let hi = meths[1] * 2 + 1;
    while (li < hi) {
      const mi = ((li + hi) >> 1) | 1;
      if (tag < meths[mi + 1]) {
        hi = mi - 2;
      } else {
        li = mi;
      }
    }
    global.caml_method_cache[cacheid] = li - 3;
    return meths[li];
  };

  global.caml_get_public_method = function caml_get_public_method(
    obj,
    tag,
    cacheid,
  ) {
    const meths = obj[1];
    if (cacheid !== undefined) {
      const ofs = global.caml_method_cache[cacheid];
      if (ofs === undefined) {
        for (let i = global.caml_method_cache.length; i < cacheid; i += 1) {
          global.caml_method_cache[i] = 0;
        }
      } else if (meths[ofs] === tag) {
        return meths[ofs - 1];
      }
    }
    let li = 3;
    let hi = meths[1] * 2 + 1;
    while (li < hi) {
      const mi = ((li + hi) >> 1) | 1;
      if (tag < meths[mi + 1]) {
        hi = mi - 2;
      } else {
        li = mi;
      }
    }
    if (cacheid !== undefined) {
      global.caml_method_cache[cacheid] = li + 1;
    }
    return tag === meths[li + 1] ? meths[li] : 0;
  };

  global.compiler_float32_neg = function compiler_float32_neg(value) {
    return float32ToBits(-bitsToFloat32(value));
  };

  global.compiler_float32_neg_boxed = global.compiler_float32_neg;

  global.compiler_float32_abs = function compiler_float32_abs(value) {
    return float32ToBits(Math.abs(bitsToFloat32(value)));
  };

  global.compiler_float32_abs_boxed = global.compiler_float32_abs;

  global.compiler_float32_add = function compiler_float32_add(left, right) {
    return float32ToBits(bitsToFloat32(left) + bitsToFloat32(right));
  };

  global.compiler_float32_add_boxed = global.compiler_float32_add;

  global.compiler_float32_sub = function compiler_float32_sub(left, right) {
    return float32ToBits(bitsToFloat32(left) - bitsToFloat32(right));
  };

  global.compiler_float32_sub_boxed = global.compiler_float32_sub;

  global.compiler_float32_mul = function compiler_float32_mul(left, right) {
    return float32ToBits(bitsToFloat32(left) * bitsToFloat32(right));
  };

  global.compiler_float32_mul_boxed = global.compiler_float32_mul;

  global.compiler_float32_div = function compiler_float32_div(left, right) {
    return float32ToBits(bitsToFloat32(left) / bitsToFloat32(right));
  };

  global.compiler_float32_div_boxed = global.compiler_float32_div;

  global.compiler_float32_mod = function compiler_float32_mod(left, right) {
    return float32ToBits(bitsToFloat32(left) % bitsToFloat32(right));
  };

  global.compiler_float32_mod_boxed = global.compiler_float32_mod;

  global.compiler_float32_compare = function compiler_float32_compare(left, right) {
    return compareFloat32Bits(left, right) | 0;
  };

  global.compiler_float32_compare_boxed = global.compiler_float32_compare;

  global.compiler_float32_equal = function compiler_float32_equal(left, right) {
    return bitsToFloat32(left) === bitsToFloat32(right) ? 1 : 0;
  };

  global.compiler_float32_equal_boxed = global.compiler_float32_equal;

  global.compiler_float32_of_float = function compiler_float32_of_float(value) {
    return float32ToBits(value);
  };

  global.compiler_float32_of_float_boxed = global.compiler_float32_of_float;

  global.compiler_float32_to_float = function compiler_float32_to_float(value) {
    return bitsToFloat32(value);
  };

  global.compiler_float32_to_float_boxed = global.compiler_float32_to_float;

  global.compiler_float32_of_string = function compiler_float32_of_string(source) {
    return float32ToBits(global.caml_float_of_string(source));
  };

  global.compiler_float32_format = function compiler_float32_format(format, value) {
    return global.caml_format_float(format, bitsToFloat32(value));
  };
})();
