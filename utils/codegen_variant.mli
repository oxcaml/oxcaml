(******************************************************************************
 *                                  OxCaml                                    *
 *                        Jacob Van Buren, Jane Street                        *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** A codegen variant is the tuple of whole-program code generation axis
    values selected for a native compilation or link.  Units compiled for
    different variants must never be mixed within one image. *)

type t =
  { frame_pointers : bool;
    poll_insertion : bool;
    stack_checks : bool
  }

(** The variant selected by the current invocation's flags.  Always
    [default] when the target is JSIR (js_of_ocaml or wasm_of_ocaml): the
    axes describe native code generation only, and the JSIR libraries exist
    only in the default library root. *)
val current : unit -> t

(** The variant baked in at configure time. *)
val default : t

val equal : t -> t -> bool

val is_default : t -> bool

(** Sets every per-invocation axis flag ([Clflags.frame_pointers],
    [Clflags.poll_insertion], [Clflags.stack_checks]) to [t]'s values, for
    tools that select a whole variant by name. *)
val apply : t -> unit

(** Canonical name encoding absolute axis values ("fp" / "base"); the
    scheme lives in [Codegen_variant_name]. *)
val to_string : t -> string

val of_string : string -> t option

(** The suffix of the runtime archives [t] links ("" for archives shared
    with the default variant): the projection of [t]'s name onto the
    runtime axes, relative to the default's; see
    [Codegen_variant_name.runtime_archive_suffix]. *)
val runtime_archive_suffix : t -> string

(** The stride, in bytes, at which the backend probes large stack frames of
    the variant selected by the current invocation's flags, so that a frame
    big enough to step over a stack's guard page faults in the guard page
    instead; zero under stack checks, where stacks have no guard page (and
    limit checks make probing unnecessary).  Reads the flags at every call;
    callers must not capture the result at module-initialization time. *)
val stack_guard_stride : unit -> int

(** The marker recorded in native compilation artifacts.  Besides the
    per-invocation axes of [t] it includes configure-time settings
    (address sanitizer) whose mixing across installs must also be
    rejected.  Poll insertion is deliberately not a key: mixing polled and
    non-polled units is sound, so polledness is tracked by the separate
    per-unit [ui_polled] bit ANDed at link time instead of by equality. *)
type fingerprint = (string * string) list

val fingerprint : t -> fingerprint

val current_fingerprint : unit -> fingerprint

val equal_fingerprint : fingerprint -> fingerprint -> bool

(** Renders e.g. "frame_pointers=true, stack_checks=false,
    address_sanitizer=false", for use in error messages and
    [ocamlobjinfo]. *)
val string_of_fingerprint : fingerprint -> string

(** The standard-library root serving the codegen variant selected by the
    current invocation's flags: [Config.standard_library] (which already
    honors OCAMLLIB/CAMLLIB) for the install's default variant;
    [Config.standard_library]/variants/<name> when that directory exists;
    and [Config.standard_library] unchanged otherwise (the base root may
    itself be a variant tree, e.g. via OCAMLLIB during the in-tree build —
    mistakes are caught by the artifact-fingerprint and runtime-archive
    checks).  The flags are read at every call (they can change between
    files via OCAMLPARAM), so callers must not capture the result at
    module-initialization time. *)
val effective_standard_library : unit -> string

(** The directory that would serve [t]'s standard library when [t] is not
    the default, [Config.standard_library]/variants/<name>, whether or not
    it exists; callers wanting the fallback semantics described above use
    [effective_standard_library]. *)
val variant_root : t -> string

(** A '+'-directory that the selected non-default variant's library tree
    does not provide while the default root does: expanding it against the
    variant root would silently resolve to a nonexistent directory even
    though a directory of that name exists one root over.  The error
    printer is registered in [Compmisc]. *)
type directory_error =
  { directory : string;  (** as written, e.g. "+compiler-libs" *)
    variant : string;
    variant_root : string
  }

exception Directory_not_provided_by_variant of directory_error

(** [Misc.expand_directory] against [effective_standard_library], resolving
    the root only when [s] actually starts with '+'.  For directories the
    caller asked to read from (-I, -H, [#directory], ...): raises
    [Directory_not_provided_by_variant] when the selected variant's tree
    exists but lacks the subdirectory while the default root provides it;
    directories absent from both roots expand permissively as always (a
    nonexistent include directory is not an error). *)
val expand_directory : string -> string

(** Like [expand_directory], but never raises: the plain expansion within
    the selected root.  For '+'-directories the caller did not ask to read
    from, such as optional search-path entries the tools add on their own
    (which must not turn into errors when the selected variant's tree
    happens to lack them) and directories being removed from the path. *)
val expand_directory_unchecked : string -> string

(** When a non-default variant is selected but the base root has no tree for
    it, a note to append to variant-related errors pointing at the probable
    cause; [None] otherwise. *)
val missing_root_hint : unit -> string option
