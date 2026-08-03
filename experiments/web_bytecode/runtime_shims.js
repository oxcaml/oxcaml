(() => {
  const global = globalThis;
  const bitsBuffer = new ArrayBuffer(4);
  const bitsI32 = new Int32Array(bitsBuffer);
  const bitsF32 = new Float32Array(bitsBuffer);
  function noteShimCall(_name) {}

  // Dox execution tracing.  The native runtime writes the same records to a
  // file; in a browser the records live in memory for the duration of one
  // evaluation.  Keep this implementation deliberately independent of the
  // playground UI so embedders can consume the exact compiler protocol.
  const doxTraceLimit = 12_000_000;
  let doxTraceBytes = 0;
  let doxTraceTruncated = false;
  let doxTraceLines = [];
  let doxCounter = 0;
  let doxHandoffCounter = 0;
  let doxStack = [];
  let doxPendingTail = null;
  let doxPendingClosureId = 0;
  let doxClosureCounter = 0;
  let doxClosures = new WeakMap();
  let doxFunctionConsumption = new WeakMap();
  const doxEnvironment = new Map();

  global.doxSetEnv = function doxSetEnv(name, value) {
    const key = String(name);
    const text = String(value);
    global.jsoo_env ??= {};
    if (text === "") {
      doxEnvironment.delete(key);
      delete global.jsoo_env[key];
    } else {
      doxEnvironment.set(key, text);
      global.jsoo_env[key] = text;
    }
  };

  function doxBytes(value) {
    if (typeof global.caml_jsbytes_of_string === "function") {
      return global.caml_jsbytes_of_string(value);
    }
    return String(value);
  }

  function doxHex(value) {
    const bytes = typeof value === "string" ? value : doxBytes(value);
    let output = "";
    for (let index = 0; index < bytes.length; index += 1) {
      output += (bytes.charCodeAt(index) & 0xff).toString(16).padStart(2, "0");
    }
    return output;
  }

  function doxFields(metadata) {
    return doxBytes(metadata).split("\x1f");
  }

  function doxShouldTrace(metadata) {
    const path = doxFields(metadata)[3] || "";
    return path.endsWith(".ml.md") || path.startsWith("<dox-inline:");
  }

  function doxIsBlock(value) {
    return value !== null && typeof value === "object";
  }

  function doxTag(value) {
    if (!doxIsBlock(value)) return -1;
    if (Array.isArray(value)) return value[0] | 0;
    return Number.isInteger(value.t) ? value.t : 0;
  }

  function doxSize(value) {
    if (!doxIsBlock(value)) return 0;
    if (Array.isArray(value)) return Math.max(0, value.length - 1);
    if (Array.isArray(value.c)) return value.c.length;
    return 0;
  }

  function doxField(value, index) {
    if (Array.isArray(value)) return value[index + 1];
    return value?.c?.[index];
  }

  function doxString(value) {
    try {
      return doxBytes(value);
    } catch (_error) {
      return null;
    }
  }

  function doxSchemaNumber(state, separator) {
    let value = 0;
    while (state.at < state.text.length && /[0-9]/.test(state.text[state.at])) {
      value = value * 10 + Number(state.text[state.at++]);
    }
    if (state.text[state.at] === separator) state.at += 1;
    return value;
  }

  function doxSchemaName(state) {
    const length = doxSchemaNumber(state, ":");
    const name = state.text.slice(state.at, state.at + length);
    state.at += length;
    return name;
  }

  function doxSkipSchema(state) {
    const kind = state.text[state.at++];
    if ("LOARFE".includes(kind)) doxSkipSchema(state);
    else if (kind === "M") {
      doxSkipSchema(state);
      doxSkipSchema(state);
    } else if (kind === "T") {
      const count = doxSchemaNumber(state, ":");
      for (let index = 0; index < count; index += 1) doxSkipSchema(state);
    } else if (kind === "Q") {
      const count = doxSchemaNumber(state, ":");
      for (let index = 0; index < count; index += 1) {
        doxSchemaName(state);
        doxSkipSchema(state);
      }
    } else if (kind === "V") {
      const constants = doxSchemaNumber(state, ":");
      for (let index = 0; index < constants; index += 1) doxSchemaName(state);
      const blocks = doxSchemaNumber(state, ":");
      for (let index = 0; index < blocks; index += 1) {
        doxSchemaNumber(state, ",");
        doxSchemaName(state);
        const arity = doxSchemaNumber(state, ":");
        for (let field = 0; field < arity; field += 1) doxSkipSchema(state);
      }
    }
  }

  function doxPreviewDynamic(value, depth = 0) {
    if (depth > 7) return { display: "…", complete: false };
    if (typeof value === "number") return { display: String(value), complete: true };
    if (typeof value === "function") return { display: "<function>", complete: false };
    const string = doxString(value);
    if (string !== null && !Array.isArray(value)) {
      const shown = string.slice(0, 240).replace(/[\x00-\x1f\x7f]/g, ".");
      return {
        display: JSON.stringify(shown) + (string.length > 240 ? "…" : ""),
        complete: string.length <= 240,
      };
    }
    if (!doxIsBlock(value)) return { display: "<opaque>", complete: false };
    const shown = Math.min(doxSize(value), 12);
    const fields = [];
    let complete = shown === doxSize(value);
    for (let index = 0; index < shown; index += 1) {
      const child = doxPreviewDynamic(doxField(value, index), depth + 1);
      fields.push(child.display);
      complete &&= child.complete;
    }
    if (shown < doxSize(value)) fields.push("…");
    return { display: `#${doxTag(value)}(${fields.join(", ")})`, complete };
  }

  function doxPreviewSchema(state, value, depth = 0, self = null) {
    const start = state.at;
    if (depth > 7 || start >= state.text.length) {
      doxSkipSchema(state);
      return { display: "…", complete: false };
    }
    const kind = state.text[state.at++];
    if (kind === "I") return { display: String(value), complete: typeof value === "number" };
    if (kind === "B") return { display: value === 0 ? "false" : "true", complete: typeof value === "number" };
    if (kind === "U") return { display: "()", complete: true };
    if (kind === "C") return { display: `'${String.fromCharCode(value | 0)}'`, complete: typeof value === "number" };
    if (kind === "D") return { display: String(value), complete: typeof value === "number" };
    if (kind === "S") {
      const string = doxString(value);
      if (string === null) return { display: "<opaque>", complete: false };
      const shown = string.slice(0, 240).replace(/[\x00-\x1f\x7f]/g, ".");
      return { display: JSON.stringify(shown) + (string.length > 240 ? "…" : ""), complete: string.length <= 240 };
    }
    if (kind === "F") {
      doxSkipSchema(state);
      return { display: "<function>", complete: false };
    }
    if (kind === "X") {
      if (!self) return { display: "<opaque>", complete: false };
      const nested = { text: self, at: 0 };
      return doxPreviewSchema(nested, value, depth + 1, self);
    }
    if (kind === "?") return doxPreviewDynamic(value, depth);
    if (kind === "Z") return doxPreviewDynamic(value, depth);
    if (kind === "T") {
      const count = doxSchemaNumber(state, ":");
      const values = [];
      let complete = doxIsBlock(value) && doxSize(value) >= count;
      for (let index = 0; index < count; index += 1) {
        const child = doxPreviewSchema(state, doxField(value, index), depth + 1, self);
        values.push(child.display);
        complete &&= child.complete;
      }
      return { display: `(${values.join(", ")})`, complete };
    }
    if (kind === "L") {
      const elementStart = state.at;
      const skip = { text: state.text, at: state.at };
      doxSkipSchema(skip);
      state.at = skip.at;
      const values = [];
      let current = value;
      let complete = true;
      while (doxIsBlock(current) && doxTag(current) === 0 && doxSize(current) === 2 && values.length < 12) {
        const childState = { text: state.text.slice(elementStart, skip.at), at: 0 };
        const child = doxPreviewSchema(childState, doxField(current, 0), depth + 1, self);
        values.push(child.display);
        complete &&= child.complete;
        current = doxField(current, 1);
      }
      if (current !== 0) { values.push("…"); complete = false; }
      return { display: `[${values.join("; ")}]`, complete };
    }
    if (kind === "O" || kind === "R") {
      const elementStart = state.at;
      doxSkipSchema(state);
      if (kind === "O" && value === 0) return { display: "None", complete: true };
      if (!doxIsBlock(value) || doxSize(value) < 1) return { display: "<opaque>", complete: false };
      const childState = { text: state.text.slice(elementStart, state.at), at: 0 };
      const child = doxPreviewSchema(childState, doxField(value, 0), depth + 1, self);
      return kind === "O"
        ? { display: `Some (${child.display})`, complete: child.complete }
        : { display: `{contents = ${child.display}}`, complete: child.complete };
    }
    if (kind === "A") {
      const elementStart = state.at;
      doxSkipSchema(state);
      if (!doxIsBlock(value)) return { display: "[|<opaque>|]", complete: false };
      const shown = Math.min(doxSize(value), 12);
      const values = [];
      let complete = shown === doxSize(value);
      for (let index = 0; index < shown; index += 1) {
        const childState = { text: state.text.slice(elementStart, state.at), at: 0 };
        const child = doxPreviewSchema(childState, doxField(value, index), depth + 1, self);
        values.push(child.display);
        complete &&= child.complete;
      }
      if (!complete && shown < doxSize(value)) values.push("…");
      return { display: `[|${values.join("; ")}|]`, complete };
    }
    if (kind === "Q") {
      const count = doxSchemaNumber(state, ":");
      const fields = [];
      let complete = doxIsBlock(value) && doxSize(value) >= count;
      for (let index = 0; index < count; index += 1) {
        const name = doxSchemaName(state);
        const child = doxPreviewSchema(state, doxField(value, index), depth + 1, state.text.slice(start));
        fields.push(`${name} = ${child.display}`);
        complete &&= child.complete;
      }
      return { display: `{${fields.join("; ")}}`, complete };
    }
    if (kind === "V") {
      const constants = doxSchemaNumber(state, ":");
      const constantNames = [];
      for (let index = 0; index < constants; index += 1) constantNames.push(doxSchemaName(state));
      const blocks = doxSchemaNumber(state, ":");
      let matched = typeof value === "number" ? constantNames[value] : null;
      let complete = matched !== undefined && matched !== null;
      for (let index = 0; index < blocks; index += 1) {
        const tag = doxSchemaNumber(state, ",");
        const name = doxSchemaName(state);
        const arity = doxSchemaNumber(state, ":");
        const values = [];
        const thisBlock = !matched && doxIsBlock(value) && doxTag(value) === tag && doxSize(value) >= arity;
        for (let field = 0; field < arity; field += 1) {
          const child = doxPreviewSchema(state, thisBlock ? doxField(value, field) : 0, depth + 1, state.text.slice(start));
          if (thisBlock) { values.push(child.display); complete &&= child.complete; }
        }
        if (thisBlock) { matched = arity ? `${name} (${values.join(", ")})` : name; complete = true; }
      }
      return { display: matched || "<opaque>", complete: Boolean(matched) && complete };
    }
    doxSkipSchema({ text: state.text, at: start });
    return doxPreviewDynamic(value, depth);
  }

  function doxPreview(metadata, value, exception = false) {
    if (exception) return { display: "<exception>", complete: false };
    const fields = doxFields(metadata);
    const schema = fields[9] || "";
    if (schema) return doxPreviewSchema({ text: schema, at: 0 }, value, 0, schema);
    const type = fields[8] || "";
    if (typeof value === "number") {
      if (type === "unit" || type.endsWith("-> unit")) return { display: "()", complete: true };
      if (type === "bool" || type.endsWith("-> bool")) return { display: value === 0 ? "false" : "true", complete: true };
      if (type.endsWith(" option") && value === 0) return { display: "None", complete: true };
      if (type.endsWith(" list") && value === 0) return { display: "[]", complete: true };
    }
    return doxPreviewDynamic(value);
  }

  function doxEmit(phase, occurrence, parent, metadata, observed, hasObserved, detail = null) {
    if (doxTraceTruncated) return;
    const fields = doxFields(metadata);
    if (!doxShouldTrace(metadata)) return;
    const site = fields[0] || "";
    const compact = new Set(["tail-handoff", "tail-link", "call-attempt-open", "call-attempt-consumed", "activation-closure", "closure-created"]).has(phase);
    const publicMetadata = compact
      ? [site, "", "", "", "0", "0", "0", "0", ""].join("\x1f")
      : fields.slice(0, 9).join("\x1f");
    const preview = detail === null && hasObserved ? doxPreview(metadata, observed, phase === "raise") : { display: detail || "", complete: detail !== null || !hasObserved };
    const content = [phase, "0", String(occurrence), parent ? String(parent) : "", publicMetadata, preview.complete ? "1" : "0", (hasObserved || detail !== null) ? preview.display : ""].join("\x1f");
    const line = `observe\t${doxHex(site)}\t${doxHex(content)}\n`;
    if (doxTraceBytes + line.length > doxTraceLimit) {
      doxTraceLines.push("trace-truncated\tbrowser-size-limit\t\n");
      doxTraceTruncated = true;
      return;
    }
    doxTraceBytes += line.length;
    doxTraceLines.push(line);
  }

  function doxEnter(metadata, tailCapable) {
    if (!doxShouldTrace(metadata)) return 0;
    const occurrence = ++doxCounter;
    const pending = doxPendingTail;
    const parent = pending?.parent ?? (doxStack.at(-1)?.occurrence || 0);
    doxPendingTail = null;
    doxStack.push({
      occurrence,
      parent,
      tailCapable,
      overapplyParent: pending?.parent || 0,
      overapplyRemaining: pending?.remaining || 0,
    });
    doxEmit("enter", occurrence, parent, metadata, 0, false);
    const kind = doxFields(metadata)[1];
    if (kind === "call") doxEmit("call-attempt-open", occurrence, parent, metadata, 0, false);
    else if (kind === "function" && parent) {
      if (doxPendingClosureId) doxEmit("activation-closure", occurrence, parent, metadata, 0, false, String(doxPendingClosureId));
      doxEmit("call-attempt-consumed", occurrence, parent, metadata, 0, false);
      doxPendingClosureId = 0;
    }
    if (pending?.handoff) doxEmit("tail-link", occurrence, parent, metadata, 0, false, `${pending.handoff}:${pending.remaining}`);
    return occurrence;
  }

  function doxLeave(phase, metadata, occurrence, observed) {
    doxPendingTail = null;
    const frame = doxStack.at(-1);
    if (!frame || frame.occurrence !== occurrence) return 0;
    doxStack.pop();
    doxEmit(phase, occurrence, frame.parent, metadata, observed, true);
    if (doxFields(metadata)[1] === "call") {
      doxEmit(phase === "raise" ? "call-attempt-raise" : "call-attempt-return", occurrence, frame.parent, metadata, observed, true);
    }
    if (frame.overapplyParent && frame.overapplyRemaining > 0 && phase === "return") {
      const consumed = doxFunctionConsumption.get(observed) || 0;
      if (consumed > 0 && frame.overapplyRemaining >= consumed) {
        const remaining = frame.overapplyRemaining - consumed;
        const fallback = doxStack.at(-1)?.occurrence || 0;
        const handoff = ++doxHandoffCounter;
        doxEmit(
          "tail-handoff",
          frame.overapplyParent,
          fallback,
          metadata,
          0,
          false,
          `${handoff}:${remaining}`,
        );
        doxPendingTail = {
          parent: frame.overapplyParent,
          remaining,
          handoff,
        };
      }
    }
    return 0;
  }

  global.doxResetTrace = function doxResetTrace() {
    doxTraceBytes = 0;
    doxTraceTruncated = false;
    doxTraceLines = [];
    doxCounter = 0;
    doxHandoffCounter = 0;
    doxStack = [];
    doxPendingTail = null;
    doxPendingClosureId = 0;
    doxClosureCounter = 0;
    doxClosures = new WeakMap();
    doxFunctionConsumption = new WeakMap();
  };

  global.doxReadTrace = function doxReadTrace() { return doxTraceLines.join(""); };
  global.caml_doclang_observe_enter = (metadata) => doxEnter(metadata, false);
  global.caml_doclang_observe_enter_tail = (metadata) => doxEnter(metadata, true);
  global.caml_doclang_observe_parameter = (occurrence, metadata, observed) => {
    const frame = doxStack.at(-1);
    if (frame?.occurrence === occurrence) doxEmit("parameter", occurrence, frame.parent, metadata, observed, true);
    return 0;
  };
  global.caml_doclang_observe_write = (metadata, observed) => {
    const occurrence = ++doxCounter;
    doxEmit("write", occurrence, doxStack.at(-1)?.occurrence || 0, metadata, observed, true);
    return 0;
  };
  global.caml_doclang_observe_return = (metadata, occurrence, observed) => doxLeave("return", metadata, occurrence, observed);
  global.caml_doclang_observe_raise = (metadata, occurrence, observed) => doxLeave("raise", metadata, occurrence, observed);
  global.caml_doclang_observe_register_function = (fn, consumption, metadata) => {
    if (typeof fn !== "function" || !doxShouldTrace(metadata)) return 0;
    const id = ++doxClosureCounter;
    doxClosures.set(fn, { id, metadata });
    doxFunctionConsumption.set(fn, consumption | 0);
    doxEmit("closure-created", id, doxStack.at(-1)?.occurrence || 0, metadata, 0, false, "");
    return 0;
  };
  global.caml_doclang_observe_register_partial = (original, partial) => {
    if (typeof partial !== "function") return 0;
    const source = doxClosures.get(original);
    const consumption = doxFunctionConsumption.get(original);
    if (!source || !consumption) return 0;
    const id = ++doxClosureCounter;
    doxClosures.set(partial, { id, metadata: source.metadata });
    doxFunctionConsumption.set(partial, Math.max(1, consumption - 1));
    doxEmit("closure-created", id, doxStack.at(-1)?.occurrence || 0, source.metadata, 0, false, `derived:${source.id}`);
    return 0;
  };
  global.caml_doclang_observe_call_function = (fn) => {
    doxPendingClosureId = doxClosures.get(fn)?.id || 0;
    return 0;
  };
  global.caml_doclang_observe_is_registered_function = (fn, supplied) =>
    (doxFunctionConsumption.get(fn) || 0) > 0 && supplied >= doxFunctionConsumption.get(fn) ? 1 : 0;
  global.caml_doclang_observe_tail_handoff = (metadata, occurrence, fn, supplied) => {
    const frame = doxStack.at(-1);
    const consumed = doxFunctionConsumption.get(fn) || 0;
    if (!frame || frame.occurrence !== occurrence || consumed <= 0 || supplied < consumed) return 0;
    const handoff = ++doxHandoffCounter;
    const remaining = supplied - consumed;
    doxEmit("tail-handoff", occurrence, frame.parent, metadata, 0, false, `${handoff}:${remaining}`);
    doxStack.pop();
    const outer = doxStack.at(-1);
    if (outer?.tailCapable) {
      const outerHandoff = ++doxHandoffCounter;
      doxEmit(
        "tail-handoff",
        outer.occurrence,
        outer.parent,
        metadata,
        0,
        false,
        `${outerHandoff}:0`,
      );
      doxEmit(
        "tail-link",
        occurrence,
        outer.occurrence,
        metadata,
        0,
        false,
        `${outerHandoff}:0`,
      );
      doxStack.pop();
    }
    doxPendingTail = { parent: occurrence, remaining, handoff };
    return 0;
  };

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
    const key = String(name);
    if (doxEnvironment.has(key)) {
      const value = doxEnvironment.get(key);
      if (typeof global.caml_string_of_jsbytes === "function") {
        return [0, global.caml_string_of_jsbytes(value)];
      }
      if (typeof global.caml_string_of_jsstring === "function") {
        return [0, global.caml_string_of_jsstring(value)];
      }
      return [0, value];
    }
    if (
      typeof global.jsoo_sys_getenv !== "function"
    ) {
      return 0;
    }
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

  global.parallel_acquire_heartbeat = function parallel_acquire_heartbeat() {
    noteShimCall("parallel_acquire_heartbeat");
    return 0;
  };

  global.parallel_release_heartbeat = function parallel_release_heartbeat() {
    noteShimCall("parallel_release_heartbeat");
    return 0;
  };

  global.parallel_setup_heartbeat = function parallel_setup_heartbeat() {
    noteShimCall("parallel_setup_heartbeat");
    return 0;
  };

  global.parallel_create_dynamic = function parallel_create_dynamic(initialValue) {
    noteShimCall("parallel_create_dynamic");
    return { oxcamlDynamicValue: initialValue };
  };

  global.parallel_unsafe_set_dynamic = function parallel_unsafe_set_dynamic(
    slot,
    value,
  ) {
    noteShimCall("parallel_unsafe_set_dynamic");
    if (slot && typeof slot === "object") {
      slot.oxcamlDynamicValue = value;
    }
    return 0;
  };

  global.caml_native_pointer_of_value_bytecode =
    function caml_native_pointer_of_value_bytecode(value) {
      noteShimCall("caml_native_pointer_of_value_bytecode");
      return { oxcamlNativePointerValue: value };
    };

  global.caml_native_pointer_of_value = global.caml_native_pointer_of_value_bytecode;

  global.caml_native_pointer_to_value_bytecode =
    function caml_native_pointer_to_value_bytecode(pointer) {
      noteShimCall("caml_native_pointer_to_value_bytecode");
      if (
        pointer &&
        typeof pointer === "object" &&
        Object.prototype.hasOwnProperty.call(pointer, "oxcamlNativePointerValue")
      ) {
        return pointer.oxcamlNativePointerValue;
      }
      return 0;
    };

  global.caml_native_pointer_to_value = global.caml_native_pointer_to_value_bytecode;

  global.caml_ext_pointer_as_native_pointer_bytecode =
    function caml_ext_pointer_as_native_pointer_bytecode(value) {
      noteShimCall("caml_ext_pointer_as_native_pointer_bytecode");
      return value;
    };

  global.caml_ext_pointer_as_native_pointer =
    global.caml_ext_pointer_as_native_pointer_bytecode;

  global.caml_reinterpret_unboxed_int64_as_tagged_int63 =
    function caml_reinterpret_unboxed_int64_as_tagged_int63(value) {
      noteShimCall("caml_reinterpret_unboxed_int64_as_tagged_int63");
      if (typeof value === "number") {
        return value | 0;
      }
      if (
        typeof global.caml_int64_lo32 === "function" &&
        value !== null &&
        value !== undefined
      ) {
        return global.caml_int64_lo32(value) | 0;
      }
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

  global.caml_max_domain_count = function caml_max_domain_count() {
    noteShimCall("caml_max_domain_count");
    return 1;
  };

  global.caml_atomic_add_field = function caml_atomic_add_field(ref, field, increment) {
    ref[field + 1] += increment;
    return 0;
  };

  global.caml_atomic_set_field = function caml_atomic_set_field(ref, field, value) {
    ref[field + 1] = value;
    return 0;
  };

  global.caml_atomic_sub_field = function caml_atomic_sub_field(ref, field, decrement) {
    ref[field + 1] -= decrement;
    return 0;
  };

  global.caml_float_of_float32 = function caml_float_of_float32(value) {
    return value;
  };

  global.caml_float32_of_float = function caml_float32_of_float(value) {
    return Math.fround(value);
  };

  global.caml_float32_of_bits_bytecode = function caml_float32_of_bits_bytecode(bits) {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    view.setInt32(0, bits, true);
    return view.getFloat32(0, true);
  };

  const dynamicBindings = new Map();
  global.caml_dynamic_make = function caml_dynamic_make() {
    return {};
  };
  global.caml_dynamic_get = function caml_dynamic_get(binding) {
    const values = dynamicBindings.get(binding);
    return values?.length ? values[values.length - 1] : 0;
  };
  global.caml_dynamic_push = function caml_dynamic_push(binding, value) {
    const values = dynamicBindings.get(binding) ?? [];
    values.push(value);
    dynamicBindings.set(binding, values);
    return 0;
  };
  global.caml_dynamic_pop = function caml_dynamic_pop(binding) {
    const values = dynamicBindings.get(binding);
    values?.pop();
    if (!values?.length) dynamicBindings.delete(binding);
    return 0;
  };

  global.caml_succ_scannable_prefix_len = function caml_succ_scannable_prefix_len() {
    return 0;
  };

  global.caml_obj_uniquely_reachable_words =
    function caml_obj_uniquely_reachable_words() {
      global.caml_failwith("Obj.uniquely_reachable_words is not available in JavaScript");
    };

  global.caml_with_async_exns = function caml_with_async_exns(callback) {
    return global.caml_callback(callback, [0]);
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
