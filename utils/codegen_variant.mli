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

(** A codegen variant is the tuple of code generation axis values selected
    for a native compilation or link.  Frame pointers, stack checks and
    AddressSanitizer are whole-program axes: units compiled with different
    values of one of them must never be mixed within one image (see
    [fingerprint]).  Poll insertion is a per-unit axis: units compiled with
    either setting can be mixed, and at link time the setting only chooses
    between library trees (see [selected_tree]). *)

type t =
  { address_sanitizer : bool;
    frame_pointers : bool;
    poll_insertion : bool;
    stack_checks : bool
  }

(** The variant selected by the current invocation's flags.  Always
    [default] when the target is JSIR (js_of_ocaml or wasm_of_ocaml): the
    axes describe native code generation only, and the JSIR libraries exist
    only in the default library root. *)
val current : unit -> t

(** The variant that the axis flags select, whatever the target: what the
    native backend generates code for when it runs.  It differs from
    [current] when the target is JSIR, so this is the one to compare with
    the variant of a program that compiles native code for itself. *)
val flags_variant : unit -> t

(** The base variant, with every axis off: what is compiled for when no
    flag and no OCAMLPARAM setting selects another, and what the library
    root holds, on every installation. *)
val default : t

val equal : t -> t -> bool

val is_default : t -> bool

(** Sets every per-invocation axis flag ([Clflags.address_sanitizer],
    [Clflags.frame_pointers], [Clflags.poll_insertion],
    [Clflags.stack_checks]) to [t]'s values, for tools that select a whole
    variant by name. *)
val apply : t -> unit

(** Canonical name encoding absolute axis values ("fp" / "base"); the
    scheme lives in [Codegen_variant_name]. *)
val to_string : t -> string

val of_string : string -> t option

(** The suffix of the runtime archives [t] links ("" for the archives of
    the base variant, which a variant that differs from it in poll
    insertion alone shares): the projection of [t]'s name onto the runtime
    axes; see [Codegen_variant_name.runtime_archive_suffix]. *)
val runtime_archive_suffix : t -> string

(** The link-time guard symbol of [t]; see
    [Codegen_variant_name.runtime_guard_symbol]. *)
val runtime_guard_symbol : t -> string

(** Whether native code can be compiled and linked for [t]: [Error message]
    when this target does not support the value that [t] gives to an axis,
    or when [t] combines axes that cannot be combined.  The message names
    the flags and the OCAMLPARAM keys to change.  Only [t] is looked at:
    what a particular backend requires of the variant is for the driver to
    check ([Oxcaml_flags.validate_codegen_variant]). *)
val check_supported : t -> (unit, string) result

(** The stride, in bytes, at which the backend probes large stack frames of
    the variant selected by the current invocation's flags, so that a frame
    big enough to step over a stack's guard page faults in the guard page
    instead; zero under stack checks, where stacks have no guard page (and
    limit checks make probing unnecessary).  Reads the flags at every call;
    callers must not capture the result at module-initialization time. *)
val stack_guard_stride : unit -> int

(** The marker recorded in native compilation artifacts: the per-invocation
    whole-program axes of [t].  Poll insertion is deliberately not a key:
    mixing polled and non-polled units is sound, so polledness is tracked
    by the separate per-unit [ui_polled] bit ANDed at link time instead of
    by equality. *)
type fingerprint = (string * string) list

val fingerprint : t -> fingerprint

val current_fingerprint : unit -> fingerprint

val equal_fingerprint : fingerprint -> fingerprint -> bool

(** Renders e.g. "frame_pointers=true, stack_checks=false,
    address_sanitizer=false", for use in error messages and
    [ocamlobjinfo]. *)
val string_of_fingerprint : fingerprint -> string

(** The library tree of this installation that serves the codegen variant
    selected by the current invocation's flags, with the variant that the
    tree was built for; [None] when the installation has no such tree.

    The tree of the default variant is [Config.standard_library] (which
    already honors OCAMLLIB/CAMLLIB), that of any other variant is
    [Config.standard_library]/variants/<name>, when that directory exists.
    Poll insertion is a property of each unit, not of the program: two trees
    that differ only in it have the same interfaces, implementation CRCs and
    symbols, and units compiled with either setting link against either.
    So the tree that serves a variant is its own when the installation has
    it, and otherwise the tree of the variant with the other poll-insertion
    setting.  This is the rule for compiling and for linking alike: at link
    time the poll-insertion flags thus choose between the libraries with
    polls and those without, where both are installed.

    The flags are read at every call (they can change between files via
    OCAMLPARAM), so callers must not capture the result at
    module-initialization time. *)
val selected_tree : unit -> (t * string) option

(** The root of [selected_tree ()], and [Config.standard_library] when
    there is none (the base root may itself be a variant tree, e.g. via
    OCAMLLIB during the in-tree build; mistakes are caught by the
    artifact-fingerprint and runtime-archive checks).  Like
    [selected_tree], not to be captured at module-initialization time. *)
val effective_standard_library : unit -> string

(** The directory that would serve [t]'s standard library when [t] is not
    the default, [Config.standard_library]/variants/<name>, whether or not
    it exists; callers wanting the fallback semantics described above use
    [effective_standard_library]. *)
val variant_root : t -> string

(** The counterpart of [path] in the library tree of the selected variant.

    Build systems name the libraries of the installation by path inside the
    default root, the one [-where] and [-config] report whatever the variant
    (<root>/unix, <root>/unix/unix.cmxa).  When the tree that serves the
    selected variant ([selected_tree]) is not the default root and [path]
    lies inside [Config.standard_library], this is the same path inside
    [effective_standard_library ()], provided it exists there; in every
    other case it is [path] itself.  So nothing changes when the default
    root serves the selected variant or when no tree does, and a path
    with no counterpart in the selected tree still names the default
    root's file, for the fingerprint and consistency checks to accept or
    reject.

    A path is compared with the root as a file is, by its absolute path,
    so it does not matter how either is spelled: a relative path is taken
    from the current directory, and repeated and trailing separators and
    "." components are ignored.  Symbolic links are not resolved, so a root
    reached under another name is not recognized, and neither is a path
    that comes back to the root through a ".." component.  A path below
    <root>/variants already names a variant's tree and is left alone, as is
    one with a ".." component below the root, and a file that the
    invocation wrote ([is_product]).

    The result is the path of a file or of a directory, to be used as it
    is: it is not a name to give to [Load_path.find], which would search
    for it again when it is relative.  A name is looked for first, as it
    is written, and what is found is mapped ([Compmisc.find_input]). *)
val in_selected_root : string -> string

(** [in_selected_root], with the selected variant read once, when the
    function is applied to [()]: for mapping many paths while the selection
    cannot change. *)
val selected_root_mapping : unit -> string -> string

(** Records that this invocation wrote the file [path]: the .cmx file of a
    unit, the interface file that compiling a .mli file or a unit without
    one writes, the object of a C file.  The units that the same invocation
    compiles afterwards and the link read such a file where it is, and not
    what the selected variant's tree has under the same name:
    [in_selected_root] leaves it alone, however its path is spelled when it
    is read back.  Only that file is recorded, not the others that could be
    named after the same source. *)
val record_product : string -> unit

(** Whether [path] names a file that [record_product] recorded
    ([same_file]). *)
val is_product : string -> bool

(** Whether [path] is a file that this invocation wrote ([is_product]) in a
    directory that [in_selected_root] replaces by another: a directory of
    the default library root that the tree of the selected variant also
    has.  That is the one case in which a search does not find such a file
    where it was written: the other directory is searched in its place, and
    can have a file of that name.  A directory of the root without a
    counterpart, and one inside a variant's tree, are searched as they
    are. *)
val is_product_in_replaced_directory : string -> bool

(** Whether two paths name the same file, as far as their spelling tells:
    a relative path is taken from the current directory, and repeated and
    trailing separators and "." components are ignored.  Symbolic links
    are not resolved and ".." components are not followed, since the
    directory before one can be a link: two paths that differ in them are
    taken for different files. *)
val same_file : string -> string -> bool

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

(** [Misc.expand_directory] against [effective_standard_library] when [s]
    starts with '+', and [in_selected_root s] otherwise.  For directories
    the caller asked to read from (-I, -H, [#directory], ...): raises
    [Directory_not_provided_by_variant] when [s] starts with '+' and the
    selected variant's tree exists but lacks the subdirectory while the
    default root provides it; directories absent from both roots expand
    permissively as always (a nonexistent include directory is not an
    error). *)
val expand_directory : string -> string

(** Like [expand_directory], but never raises: the plain expansion within
    the selected root.  For '+'-directories the caller did not ask to read
    from, such as optional search-path entries the tools add on their own
    (which must not turn into errors when the selected variant's tree
    happens to lack them) and directories being removed from the path. *)
val expand_directory_unchecked : string -> string

(** When the installation has no tree that serves the selected variant
    ([selected_tree] is [None]), a note to append to variant-related errors
    pointing at the probable cause; [None] otherwise. *)
val missing_root_hint : unit -> string option
