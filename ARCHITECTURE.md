# OxCaml Architecture

This document describes the high-level architecture of the OxCaml compiler, a performance-focused version of OCaml that serves as the development home for the Flambda2 optimizer and CFG backend.

If you want to familiarize yourself with the codebase, you are in the right place!

## Bird's Eye View

**Problem**: OCaml's standard compiler produces good code but leaves significant performance on the table. Real-world OCaml applications often hit performance bottlenecks due to limited optimization, particularly around function inlining, memory allocation, and register allocation.

**Solution**: OxCaml extends the standard OCaml compiler with aggressive optimization infrastructure:

1. **Flambda2**: Advanced middle-end optimizer with sophisticated inlining, specialization, and data flow analysis
2. **CFG Backend**: Control flow graph-based code generation with modern register allocation algorithms

The compilation pipeline follows: **Source → Parse → Type → Lambda → Flambda2 → CMM → CFG → Linear → Assembly**

**Architecture Invariant**: Each compilation phase communicates through well-defined intermediate representations, enabling independent development of optimization passes.

## Code Map

The OxCaml codebase is organized into several key directories:

### Frontend (Language Analysis)
- `parsing/`: Lexical analysis, parsing, and AST construction (`lexer.mll`, `parser.mly`, `parsetree.mli`)
- `typing/`: Type inference and checking (`typecore.ml`, `typemod.ml`, `typedtree.mli`)

### Middle-end (Optimization)
- `lambda/`: High-level IR translation and pattern matching compilation
  - Converts typed AST to Lambda IR (`lambda.mli`)
  - Performs closure conversion and pattern matching compilation
- `middle_end/flambda2/`: The Flambda2 optimizer (OxCaml's key innovation)
  - **Core Infrastructure**: 
    - `terms/`: Flambda2 IR definition with explicit continuation-passing style
    - `types/`: Sophisticated type system with meet/join lattices for optimization
    - `identifiers/`: Variable, continuation, and symbol management
  - **Optimization Engine**:
    - `simplify/`: Main optimization passes including inlining, specialization, and unboxing
    - `simplify/flow/`: Data flow analysis and control flow graph construction  
    - `simplify/inlining/`: Function inlining heuristics and transformations
  - **Code Generation**: `to_cmm/` translates optimized Flambda2 to CMM IR

**Architecture Invariant**: Lambda IR is the boundary between frontend and middle-end. All type information from the frontend is preserved through this interface.

### Backend (Code Generation)
- `backend/`: CMM to assembly compilation with CFG-based optimizations
  - **CFG Infrastructure**: `cfg/` provides control flow graph representation
    - `cfg_dataflow.ml`: Generic data flow analysis framework
    - `cfg_liveness.ml`: Register liveness analysis for allocation
    - Multiple register allocation algorithms in `regalloc/` (IRC, linear scan, greedy)
  - **Architecture Backends**: `amd64/` and `arm64/` handle instruction selection and emission
    - Architecture-specific instruction selection and SIMD vectorization
    - Stack management and calling conventions
  - **Analysis and Optimization**: Peephole optimization, branch relaxation, and debug info generation
- `asmcomp/`: Native code compilation coordination and linking
- `bytecomp/`: Bytecode compilation pipeline with a different optimization profile

**Architecture Invariant**: The backend operates on architecture-neutral CMM, with architecture-specific details isolated to dedicated subdirectories.

### Supporting Infrastructure
- `runtime/` & `runtime4/`: OCaml 5 and OCaml 4 runtime systems, respectively
  - Garbage collector, memory management, and system interface
  - `runtime/` includes multicore support and effect handlers
- `stdlib/` & `otherlibs/`: Standard library and additional libraries (`str`, `unix`, `dynlink`)
- `driver/`: Compiler entry points and command-line processing
  - `main.ml`: Bytecode compiler, `optmain.ml`: Native compiler
  - `oxcaml_main.ml`: OxCaml-specific extensions and flags
- `utils/`: Shared utilities, configuration management, and algorithms
- `tools/`: Development and analysis tools
  - `ocamldep`: Dependency analysis, `objinfo`: Object file inspection
  - `chamelon/`: Test case minimizer for compiler bug isolation
- `toplevel/`: Interactive REPL sharing the frontend with batch compilers
- `debugger/`: Source-level debugger with OCaml 4 and OCaml 5 runtime support
- `file_formats/`: Binary format definitions enabling separate compilation and cross-module optimization

## Cross-Cutting Concerns

### Intermediate Representations
The compilation pipeline uses a sequence of IRs, each designed for specific analysis and optimization:

- **AST**: Untyped syntax tree from parsing (`parsetree.mli`)
- **Typedtree**: Typed AST with full type information (`typedtree.mli`) 
- **Lambda**: High-level functional IR with closure conversion (`lambda.mli`)
- **Flambda2**: CPS-based IR with a precise type system for aggressive optimization
- **CMM**: Architecture-neutral C-- for portable code generation (`cmm.mli`)
- **CFG**: Control flow graph for backend analysis and register allocation
- **Linear**: Linearized instruction sequences from CFG (`linear.mli`)
- **Assembly**: Target-specific machine code

Each transformation preserves sufficient semantic information for the next phase while enabling phase-specific optimizations.

### Separate Compilation and Cross-Module Optimization
OxCaml maintains separate compilation while enabling cross-module optimization:
- **Interface Files**: `.cmi` files contain type information and module signatures
- **Implementation Files**: `.cmx` files contain optimization information for cross-module inlining
- **Flambda2 Extensions**: Additional cross-module information in specialized formats

### Dual Runtime Support
Architectural decision to support both OCaml 4 and OCaml 5 runtimes:
- **Runtime Selection**: Chosen at configure time with `--enable-runtime5`
- **Compiler Agnostic**: Most compilation phases work with both runtimes
- **Runtime-Specific Code**: Isolated to `runtime/` vs. `runtime4/` directories and specific system interfaces

## Key Architectural Decisions

### Flambda2 as a Separate Optimization Phase
Flambda2 operates as an independent compilation phase with its own IR and type system:
- **Design Rationale**: Enables aggressive optimizations impossible with source-level types
- **CPS-Based IR**: Explicit continuation-passing style simplifies control flow analysis
- **Precise Type System**: More detailed than frontend types, enabling specialization and unboxing
- **Cross-Module Analysis**: Rich `.cmx` format enables whole-program-style optimizations

### CFG-Based Backend
Transition from linear instruction sequences to explicit control flow graphs:
- **Motivation**: Traditional linear backends limit analysis-driven optimizations
- **Benefits**: Enables sophisticated register allocation, instruction scheduling, and loop optimization
- **Implementation**: Multiple register allocators (IRC, linear scan, greedy) with precise liveness analysis
- **SIMD Support**: CFG representation facilitates automatic vectorization

### Performance vs. Compilation Speed Trade-off
Explicit architectural choice prioritizing runtime performance:
- **Philosophy**: Accept longer compile times for significant runtime improvements
- **Implementation**: Multi-pass optimization in Flambda2 and extensive analysis in the backend
- **Configurability**: Different optimization levels allow tuning of the trade-off

## Important Entry Points

When exploring specific functionality:

- **Parsing**: Start with `parsing/parser.mly` and `parsing/lexer.mll`
- **Type checking**: Core logic in `typing/typecore.ml` and `typing/typemod.ml`
- **Lambda Translation**: Pattern compilation in `lambda/matching.ml`; closure conversion in `lambda/translcore.ml`
- **Flambda2 Optimization**: Main driver in `middle_end/flambda2/flambda2.ml`; optimization engine in `middle_end/flambda2/simplify/`
- **Backend**: CFG construction in `backend/cfg/`; architecture-specific code in `backend/amd64/` or `backend/arm64/`
- **Runtime System**: GC and primitives in `runtime/` (OCaml 5) or `runtime4/` (OCaml 4)

**Architecture Invariant**: Each compilation phase has well-defined entry points and communicates through standardized intermediate representations, enabling independent development and testing of optimization passes.