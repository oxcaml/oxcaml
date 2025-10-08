window.IKINDS_GRAPH_DATA = {
  "nodes": [
    {
      "id": "typing/ikinds/ikinds.ml",
      "label": "typing/ikinds/ikinds.ml",
      "info": "Ikinds facade: builds the LDD solver, normalizes jkinds, and exposes subkind/substitution APIs consumed by typing.*"
    },
    {
      "id": "typing/ikinds/ikind.ml",
      "label": "typing/ikinds/ikind.ml",
      "info": "Ikind core layer: defines rigid atoms and instantiates Ikind.Ldd over Axis_lattice and the chosen backend."
    },
    {
      "id": "typing/ikinds/axis_lattice.ml",
      "label": "typing/ikinds/axis_lattice.ml",
      "info": "Bitset-backed axis lattice with modality masks and canned constants; conversions live in Axis_lattice_conv."
    },
    {
      "id": "typing/ikinds/axis_lattice_conv.ml",
      "label": "typing/ikinds/axis_lattice_conv.ml",
      "info": "Conversion helpers turning Types.Jkind_mod_bounds into axis lattice levels and Mode crossings."
    },
    {
      "id": "typing/ikinds/axis_lattice_array.ml",
      "label": "typing/ikinds/axis_lattice_array.ml",
      "info": "Array-based axis lattice mirroring Jkind axis order and wrapping the generic product lattice; conversions live in Axis_lattice_array_conv."
    },
    {
      "id": "typing/ikinds/axis_lattice_array_conv.ml",
      "label": "typing/ikinds/axis_lattice_array_conv.ml",
      "info": "Bridges axis lattice arrays with Types metadata, producing Mode-aware masks for array encoders."
    },
    {
      "id": "typing/ikinds/product_lattice.ml",
      "label": "typing/ikinds/product_lattice.ml",
      "info": "Generic product lattice functor providing join/meet/co_sub over fixed axis shapes."
    },
    {
      "id": "typing/ikinds/ldd.ml",
      "label": "typing/ikinds/ldd.ml",
      "info": "Backend switchboard: defaults to the cached-down0 implementation while keeping alternative backends pluggable."
    },
    {
      "id": "typing/ikinds/ldd_cached_down0.ml",
      "label": "typing/ikinds/ldd_cached_down0.ml",
      "info": "Default cached-down0 backend: non-memoized LDD with cached down0 values and rigid-variable reuse."
    },
    {
      "id": "typing/ikinds/ldd_memo.ml",
      "label": "typing/ikinds/ldd_memo.ml",
      "info": "Memoizing LDD backend with unique tables and global counter instrumentation hooks."
    },
    {
      "id": "typing/ikinds/global_counters.ml",
      "label": "typing/ikinds/global_counters.ml",
      "info": "Global counter table used for lightweight solver profiling/instrumentation."
    },
    {
      "id": "typing/jkind.ml",
      "label": "typing/jkind.ml",
      "info": "Jkind definitions, allowance helpers, and normalization logic that Ikinds reuses."
    },
    {
      "id": "typing/jkind_axis.ml",
      "label": "typing/jkind_axis.ml",
      "info": "Axis metadata (modal and nonmodal) shared by jkinds and the lattice encoders."
    },
    {
      "id": "typing/jkind_types.ml",
      "label": "typing/jkind_types.ml",
      "info": "Jkind type definitions and sort constants used to determine constructor argument properties."
    },
    {
      "id": "typing/mode.ml",
      "label": "typing/mode.ml",
      "info": "Mode and modality crossing definitions supplying per-axis constants."
    },
    {
      "id": "typing/types.ml",
      "label": "typing/types.ml",
      "info": "Core Types module: owns type_expr, jkinds, and constructor_ikind caches consumed throughout typing and ikinds."
    },
    {
      "id": "typing/allowance.ml",
      "label": "typing/allowance.ml",
      "info": "Allowance utilities describing which jkinds may appear on left/right sides of subtyping checks."
    },
    {
      "id": "typing/btype.ml",
      "label": "typing/btype.ml",
      "info": "Basic type operations including row type analysis for variant immediacy checking."
    },
    {
      "id": "typing/path.ml",
      "label": "typing/path.ml",
      "info": "Path representation and manipulation for type constructor names, used throughout the type system."
    },
    {
      "id": "typing/ctype.ml",
      "label": "typing/ctype.ml",
      "info": "Type checker routines that defer subjkind/intersection and crossing logic to Ikinds."
    },
    {
      "id": "typing/typedecl.ml",
      "label": "typing/typedecl.ml",
      "info": "Type declaration processing that normalizes jkinds and caches constructor polynomials via Ikinds."
    },
    {
      "id": "typing/typemod.ml",
      "label": "typing/typemod.ml",
      "info": "Module typing entry that precomputes constructor ikinds for predefs when -ikinds is enabled."
    },
    {
      "id": "typing/subst.ml",
      "label": "typing/subst.ml",
      "info": "Substitution utilities that preserve constructor ikinds by remapping rigid atoms without Env access."
    },
    {
      "id": "utils/clflags.ml",
      "label": "utils/clflags.ml",
      "info": "Compiler flags controlling feature enablement, including the -ikinds flag."
    }
  ],
  "edges": [
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/ikinds/ikind.ml",
      "info": "Ikinds builds on Ikind.Ldd for lattice nodes and rigid-name helpers."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Ikinds calls Axis_lattice for masks/constants when translating jkinds."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/ikinds/axis_lattice_conv.ml",
      "info": "Ikinds converts Jkind mod-bounds through Axis_lattice_conv before solving."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/jkind.ml",
      "info": "Ikinds consumes Jkind APIs (disallow/with_bounds/modality) while constructing ckinds."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/types.ml",
      "info": "Ikinds stores constructor polynomials directly through the shared constructor_ikind record."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/path.ml",
      "info": "Uses Path.compare, Path.print, and Path.Map for constructor paths."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/btype.ml",
      "info": "Uses Btype functions (tvariant_not_immediate, static_row, fold_row) for variant analysis."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/jkind_types.ml",
      "info": "Uses Jkind_types.Sort.Const.all_void to analyze constructor arguments."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/mode.ml",
      "info": "Produces Mode.Crossing summaries from lattice results when ikinds gating is enabled."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "typing/allowance.ml",
      "info": "Subtyping helpers rely on Allowance metadata to qualify jkinds."
    },
    {
      "source": "typing/ikinds/ikinds.ml",
      "target": "utils/clflags.ml",
      "info": "Reads Clflags.ikinds flag to enable/disable the ikinds system."
    },
    {
      "source": "typing/ikinds/ikind.ml",
      "target": "typing/path.ml",
      "info": "Uses Path.compare in RigidName module for constructor ordering."
    },
    {
      "source": "typing/ikinds/ikind.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Ikind.Ldd uses the bitset lattice as its coefficient domain."
    },
    {
      "source": "typing/ikinds/ikind.ml",
      "target": "typing/ikinds/ldd.ml",
      "info": "Ikind.Ldd delegates diagram operations to the selected backend from Ldd."
    },
    {
      "source": "typing/ikinds/axis_lattice.ml",
      "target": "typing/jkind_axis.ml",
      "info": "Axis_lattice iterates Jkind_axis metadata to map axes to bit positions."
    },
    {
      "source": "typing/ikinds/axis_lattice.ml",
      "target": "typing/mode.ml",
      "info": "Axis_lattice queries Mode.Modality constants to determine relevant axes."
    },
    {
      "source": "typing/ikinds/axis_lattice_conv.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Axis_lattice_conv lifts raw lattice constants into Axis_lattice values."
    },
    {
      "source": "typing/ikinds/axis_lattice_conv.ml",
      "target": "typing/types.ml",
      "info": "Axis_lattice_conv converts Types.Jkind_mod_bounds to lattice levels."
    },
    {
      "source": "typing/ikinds/axis_lattice_conv.ml",
      "target": "typing/mode.ml",
      "info": "Axis_lattice_conv packages lattice levels back into Mode.Crossing records."
    },
    {
      "source": "typing/jkind.ml",
      "target": "typing/types.ml",
      "info": "Jkind reads Types constructor metadata (allowances and mod bounds) through the exposed APIs."
    },
    {
      "source": "typing/jkind.ml",
      "target": "typing/jkind_axis.ml",
      "info": "Jkind relies on Jkind_axis metadata when tracking with-bound relevance."
    },
    {
      "source": "typing/jkind.ml",
      "target": "typing/mode.ml",
      "info": "Jkind manipulates Mode.Crossing/Mod_bounds for modality reasoning."
    },
    {
      "source": "typing/jkind.ml",
      "target": "typing/allowance.ml",
      "info": "Jkind maintains Allowance tables reused by ikinds' subtyping wrapper."
    },
    {
      "source": "typing/ikinds/ldd.ml",
      "target": "typing/ikinds/ldd_cached_down0.ml",
      "info": "Ldd.Make routes to the cached-down0 backend by default."
    },
    {
      "source": "typing/ikinds/ldd_cached_down0.ml",
      "target": "typing/ikinds/axis_lattice.ml",
      "info": "Cached-down0 nodes store Axis_lattice masks and reuse lattice constants."
    },
    {
      "source": "typing/ikinds/ldd_memo.ml",
      "target": "typing/ikinds/global_counters.ml",
      "info": "Memo backend bumps Global_counters to track solver statistics."
    },
    {
      "source": "typing/ikinds/axis_lattice_array.ml",
      "target": "typing/ikinds/product_lattice.ml",
      "info": "Axis_lattice_array is instantiated from the generic product lattice functor."
    },
    {
      "source": "typing/ikinds/axis_lattice_array.ml",
      "target": "typing/jkind_axis.ml",
      "info": "Axis_lattice_array reuses Jkind_axis metadata for naming/ordering."
    },
    {
      "source": "typing/ikinds/axis_lattice_array.ml",
      "target": "typing/mode.ml",
      "info": "Axis_lattice_array queries Mode.Modality to compute relevant-axis masks."
    },
    {
      "source": "typing/ikinds/axis_lattice_array_conv.ml",
      "target": "typing/ikinds/axis_lattice_array.ml",
      "info": "Axis_lattice_array_conv translates array lattice levels into packed Axis_lattice_array values."
    },
    {
      "source": "typing/ikinds/axis_lattice_array_conv.ml",
      "target": "typing/types.ml",
      "info": "Axis_lattice_array_conv encodes/decodes Types.Jkind_mod_bounds arrays."
    },
    {
      "source": "typing/types.ml",
      "target": "typing/ikinds/ikind.ml",
      "info": "Types serializes constructor_ikind polynomials using Ikind.Ldd nodes."
    },
    {
      "source": "typing/types.ml",
      "target": "typing/allowance.ml",
      "info": "Types reexports allowance data while normalizing jkinds."
    },
    {
      "source": "typing/ikinds/axis_lattice_array_conv.ml",
      "target": "typing/mode.ml",
      "info": "Axis_lattice_array_conv wraps Mode.Modality constants for array conversions."
    },
    {
      "source": "typing/ctype.ml",
      "target": "typing/ikinds/ikinds.ml",
      "info": "Type checker defers subjkind/intersection and crossing checks to Ikinds."
    },
    {
      "source": "typing/typedecl.ml",
      "target": "typing/ikinds/ikinds.ml",
      "info": "Type declarations normalize jkinds and cache constructor polynomials through Ikinds."
    },
    {
      "source": "typing/typemod.ml",
      "target": "typing/ikinds/ikinds.ml",
      "info": "Typemod precomputes constructor ikinds via Ikinds when bootstrapping environments."
    },
    {
      "source": "typing/subst.ml",
      "target": "typing/ikinds/ikinds.ml",
      "info": "Identity-env edge: substitution uses Ikinds.poly_of_type_function_in_identity_env to remap polynomials."
    }
  ]
};
