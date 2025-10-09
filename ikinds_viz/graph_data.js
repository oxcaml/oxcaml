window.IKINDS_GRAPH_DATA = {
  "nodes": [
    {
      "id": "typing/ikinds/ikinds.ml",
      "label": "typing/ikinds/ikinds.ml",
      "info": "Ikinds facade: builds the solver, normalizes jkinds, and exposes subkind/substitution entry points used by typing."
    },
    {
      "id": "typing/ikinds/ikind.ml",
      "label": "typing/ikinds/ikind.ml",
      "info": "Ikind core: defines rigid names and re-exports Ikind.Ldd over Axis_lattice and the active backend."
    },
    {
      "id": "typing/ikinds/axis_lattice.ml",
      "label": "typing/ikinds/axis_lattice.ml",
      "info": "Bitset lattice for modality axes, supplying canned masks/constants to Ikinds backends."
    },
    {
      "id": "typing/ikinds/axis_lattice_conv.ml",
      "label": "typing/ikinds/axis_lattice_conv.ml",
      "info": "Converts Types.Jkind_mod_bounds records into Axis_lattice values and Mode crossings."
    },
    {
      "id": "typing/ikinds/axis_lattice_array.ml",
      "label": "typing/ikinds/axis_lattice_array.ml",
      "info": "Array-based lattice aligned with Jkind axis order, instantiated from the generic product lattice."
    },
    {
      "id": "typing/ikinds/axis_lattice_array_conv.ml",
      "label": "typing/ikinds/axis_lattice_array_conv.ml",
      "info": "Translates Types metadata to and from axis-lattice-array encodings with Mode-aware masks."
    },
    {
      "id": "typing/ikinds/product_lattice.ml",
      "label": "typing/ikinds/product_lattice.ml",
      "info": "Generic product lattice functor providing join/meet/co_sub over fixed axis shapes."
    },
    {
      "id": "typing/ikinds/ldd.ml",
      "label": "typing/ikinds/ldd.ml",
      "info": "Backend switchboard: functor that routes Ikind.Ldd to the chosen implementation (cached-down0 by default)."
    },
    {
      "id": "typing/ikinds/ldd_cached_down0.ml",
      "label": "typing/ikinds/ldd_cached_down0.ml",
      "info": "Default cached-down0 backend: ZDD with down0 caches and rigid-variable reuse, no memo tables."
    },
    {
      "id": "typing/ikinds/ldd_leaf_specialized.ml",
      "label": "typing/ikinds/ldd_leaf_specialized.ml",
      "info": "Leaf-specialized cached-down0 backend with unboxed Axis_lattice leaves for profiling experiments."
    },
    {
      "id": "typing/ikinds/ldd_no_memo.ml",
      "label": "typing/ikinds/ldd_no_memo.ml",
      "info": "Non-memoizing backend variant retaining down0 caches but dropping unique tables/memo tables."
    },
    {
      "id": "typing/ikinds/ldd_no_fast_path.ml",
      "label": "typing/ikinds/ldd_no_fast_path.ml",
      "info": "Cached-down0 backend variant with join/meet fast paths disabled for debugging baselines."
    },
    {
      "id": "typing/ikinds/ldd_unboxed.ml",
      "label": "typing/ikinds/ldd_unboxed.ml",
      "info": "Legacy unboxed backend kept to benchmark against cached-down0 implementations."
    },
    {
      "id": "typing/ikinds/ldd_memo.ml",
      "label": "typing/ikinds/ldd_memo.ml",
      "info": "Memoizing backend with unique tables and instrumentation hooks for Global_counters."
    },
    {
      "id": "typing/ikinds/global_counters.ml",
      "label": "typing/ikinds/global_counters.ml",
      "info": "Global counter storage used to instrument memoizing solver backends."
    },
    {
      "id": "typing/jkind.ml",
      "label": "typing/jkind.ml",
      "info": "Jkind solver: enforces allowances, computes mod-bounds, and exposes APIs Ikinds wraps."
    },
    {
      "id": "typing/jkind_axis.ml",
      "label": "typing/jkind_axis.ml",
      "info": "Source of modal/nonmodal axis metadata and ordering shared by Jkind and lattice encoders."
    },
    {
      "id": "typing/mode.ml",
      "label": "typing/mode.ml",
      "info": "Mode and modality definitions supplying constants and Mode.Crossing constructors."
    },
    {
      "id": "typing/types.ml",
      "label": "typing/types.ml",
      "info": "Core Types module owning type_expr/jkind representations and constructor_ikind caches."
    },
    {
      "id": "typing/path.ml",
      "label": "typing/path.ml",
      "info": "Path utilities (comparison, maps, printers) for constructor identifiers across typing."
    },
    {
      "id": "typing/ctype.ml",
      "label": "typing/ctype.ml",
      "info": "Typecore; delegates subjkind/intersection/crossing checks to Ikinds when enabled."
    },
    {
      "id": "typing/typedecl.ml",
      "label": "typing/typedecl.ml",
      "info": "Type declaration pipeline that normalizes jkinds and caches constructor polynomials via Ikinds."
    },
    {
      "id": "typing/typemod.ml",
      "label": "typing/typemod.ml",
      "info": "Module typing entry; precomputes constructor ikinds for predefs under -ikinds."
    },
    {
      "id": "typing/subst.ml",
      "label": "typing/subst.ml",
      "info": "Substitution helpers that remap constructor_ikind polynomials through Ikinds without Env access."
    },
    {
      "id": "utils/clflags.ml",
      "label": "utils/clflags.ml",
      "info": "Defines compiler flags, including the -ikinds toggle consulted by Ikinds."
    }
  ],
  "edges": [
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/ikinds/ikind.ml",
      "info": "Ikinds uses Ikind.Ldd re-exports for rigid names and nodes."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Requests modality masks/constants from Axis_lattice during jkind translation."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/ikinds/axis_lattice_conv.ml",
      "info": "Converts Types.Jkind_mod_bounds via Axis_lattice_conv before solving."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/jkind.ml",
      "info": "Wraps Jkind APIs (disallow/with_bounds/etc.) when building constructor polynomials."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/types.ml",
      "info": "Persists constructor_ikind results into Types' caches."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/path.ml",
      "info": "Relies on Path comparison/printing/Map for constructor keys."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/mode.ml",
      "info": "Calls Mode helpers to emit Mode.Crossing summaries."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "utils/clflags.ml",
      "info": "Reads Clflags.ikinds to decide whether to run."
    },
    {
      "source": "typing/ikinds/ikind.ml",
      "target": "typing/path.ml",
      "info": "RigidName uses Path.compare to order constructors."
    },
    {
      "source": "typing/ikinds/ikind.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Ikind.Ldd takes Axis_lattice as its coefficient lattice."
    },
    {
      "source": "typing/ikinds/ikind.ml",
      "target": "typing/ikinds/ldd.ml",
      "info": "Ikind.Ldd delegates ZDD operations to the backend selected by Ldd.Make."
    },
    {
      "source": "typing/ikinds/axis_lattice.ml",
      "target": "typing/jkind_axis.ml",
      "info": "Iterates Jkind_axis metadata to map axes to bit positions."
    },
    {
      "source": "typing/ikinds/axis_lattice.ml",
      "target": "typing/mode.ml",
      "info": "Queries Mode.Modality constants to classify axes."
    },
    {
      "source": "typing/ikinds/axis_lattice_conv.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Constructs Axis_lattice values from mod-bound records."
    },
    {
      "source": "typing/ikinds/axis_lattice_conv.ml",
      "target": "typing/types.ml",
      "info": "Reads and writes Types.Jkind_mod_bounds structures."
    },
    {
      "source": "typing/ikinds/axis_lattice_conv.ml",
      "target": "typing/mode.ml",
      "info": "Synthesizes Mode.Crossing records from lattice levels."
    },
    {
      "source": "typing/jkind.ml",
      "target": "typing/types.ml",
      "info": "Consumes Types' allowance and mod-bound tables."
    },
    {
      "source": "typing/jkind.ml",
      "target": "typing/jkind_axis.ml",
      "info": "References axis metadata for with-bound processing."
    },
    {
      "source": "typing/jkind.ml",
      "target": "typing/mode.ml",
      "info": "Manipulates Mode crossings and modality constants."
    },
    {
      "source": "typing/ikinds/ldd.ml",
      "target": "typing/ikinds/ldd_cached_down0.ml",
      "info": "Switchboard instantiates the cached-down0 backend by default."
    },
    {
      "source": "typing/ikinds/ldd.ml",
      "target": "typing/ikinds/ldd_memo.ml",
      "info": "Switchboard can instead target the memoizing backend."
    },
    {
      "source": "typing/ikinds/ldd.ml",
      "target": "typing/ikinds/ldd_no_memo.ml",
      "info": "Switchboard exposes the non-memo backend for experiments."
    },
    {
      "source": "typing/ikinds/ldd.ml",
      "target": "typing/ikinds/ldd_no_fast_path.ml",
      "info": "Switchboard exposes a cached-down0 build without fast paths."
    },
    {
      "source": "typing/ikinds/ldd.ml",
      "target": "typing/ikinds/ldd_unboxed.ml",
      "info": "Switchboard exposes the legacy unboxed backend."
    },
    {
      "source": "typing/ikinds/ldd.ml",
      "target": "typing/ikinds/ldd_leaf_specialized.ml",
      "info": "Switchboard exposes the leaf-specialized cached-down0 backend."
    },
    {
      "source": "typing/ikinds/ldd_cached_down0.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Cached-down0 nodes store Axis_lattice leaves and masks."
    },
    {
      "source": "typing/ikinds/ldd_memo.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Memo backend attaches Axis_lattice leaves to memoized nodes."
    },
    {
      "source": "typing/ikinds/ldd_memo.ml",
      "target": "typing/ikinds/global_counters.ml",
      "info": "Memo backend updates Global_counters when instrumentation is enabled."
    },
    {
      "source": "typing/ikinds/ldd_leaf_specialized.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Leaf-specialized backend unboxes Axis_lattice leaves."
    },
    {
      "source": "typing/ikinds/ldd_no_memo.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "No-memo backend still reads Axis_lattice for down0 caches."
    },
    {
      "source": "typing/ikinds/ldd_no_fast_path.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Fast-path-disabled backend uses the same Axis_lattice constants."
    },
    {
      "source": "typing/ikinds/ldd_unboxed.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Unboxed backend holds Axis_lattice values directly in leaves."
    },
    {
      "source": "typing/ikinds/axis_lattice_array.ml",
      "target": "typing/ikinds/product_lattice.ml",
      "info": "Axis_lattice_array instantiates the generic product lattice functor."
    },
    {
      "source": "typing/ikinds/axis_lattice_array.ml",
      "target": "typing/jkind_axis.ml",
      "info": "Uses Jkind_axis ordering for its indices."
    },
    {
      "source": "typing/ikinds/axis_lattice_array.ml",
      "target": "typing/mode.ml",
      "info": "Queries Mode.Modality when deriving masks."
    },
    {
      "source": "typing/ikinds/axis_lattice_array_conv.ml",
      "target": "typing/ikinds/axis_lattice_array.ml",
      "info": "Produces and consumes Axis_lattice_array encodings."
    },
    {
      "source": "typing/ikinds/axis_lattice_array_conv.ml",
      "target": "typing/types.ml",
      "info": "Marshals Types.Jkind_mod_bounds arrays."
    },
    {
      "source": "typing/ikinds/axis_lattice_array_conv.ml",
      "target": "typing/mode.ml",
      "info": "Packages Mode.Modality constants into array form."
    },
    {
      "source": "typing/types.ml",
      "target": "typing/ikinds/ikind.ml",
      "info": "Types materializes constructor_ikind polynomials as Ikind.Ldd nodes."
    },
    {
      "source": "typing/ctype.ml",
      "target": "typing/ikinds/ikinds.ml",
      "info": "Typecore calls Ikinds for subjkind/intersection/crossing checks."
    },
    {
      "source": "typing/typedecl.ml",
      "target": "typing/ikinds/ikinds.ml",
      "info": "Type declaration logic queries Ikinds to normalize jkinds and cache polynomials."
    },
    {
      "source": "typing/typemod.ml",
      "target": "typing/ikinds/ikinds.ml",
      "info": "Module typing asks Ikinds to precompute constructor ikinds for predefs."
    },
    {
      "source": "typing/subst.ml",
      "target": "typing/ikinds/ikinds.ml",
      "info": "Substitution remaps constructor polynomials through Ikinds without Env access."
    }
  ]
};
