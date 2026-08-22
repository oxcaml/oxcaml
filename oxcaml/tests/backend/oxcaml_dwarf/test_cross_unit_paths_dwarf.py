"""Check the DWARF file paths recorded for code inlined from other
compilation units that were compiled in different working directories.

The modules in cross_unit_dir/ are compiled from within that subdirectory,
each with a [-directory] argument identifying its source directory (see
gen/gen_dune.ml), and [@inline always] inlines the chain f_caller ->
Cu_lib_outer.outer -> Cu_lib_inner.inner into the main unit. The file of
each frame's line entry must resolve to the defining unit's real source
directory: a regression that records the foreign files' bare names, leaving
them to be resolved against the consuming unit's directory, puts the
cu_lib_*.ml frames at nonexistent .../oxcaml_dwarf/cu_lib_*.ml paths
instead of .../cross_unit_dir/cu_lib_*.ml.
"""

import os
import lldb
import lldb_test_utils

EXE = "./test_cross_unit_paths_dwarf.exe"

INLINED = True
NOT_INLINED = False


# The lib sources, as reachable from this test's directory (where the test
# runs), and the main source, which lives in this test's directory itself.
INNER_SOURCE = "cross_unit_dir/cu_lib_inner.ml"
OUTER_SOURCE = "cross_unit_dir/cu_lib_outer.ml"
MAIN_SOURCE = "test_cross_unit_paths_dwarf.ml"


def expected_frames():
    """Innermost first:
    (function name fragment, inlined?, path suffix, (line, column)).
    The path suffix is the directory-qualified form the DWARF must record,
    ending in the source directory the unit was compiled against."""
    return [
        # The innermost frame is at its own position inside [inner]'s body.
        ("inner", INLINED, INNER_SOURCE,
         lldb_test_utils.source_pos(INNER_SOURCE, "(x * 3)")),
        # Each caller frame is at the position of the corresponding call,
        # which lies in the file of the function containing that call.
        ("outer", INLINED, OUTER_SOURCE,
         lldb_test_utils.source_pos(OUTER_SOURCE,
                                    "Cu_lib_inner.inner (x + 2)")),
        ("f_caller", NOT_INLINED, "oxcaml_dwarf/" + MAIN_SOURCE,
         lldb_test_utils.source_pos(
             MAIN_SOURCE, "Cu_lib_outer.outer (Sys.opaque_identity x)")),
    ]


def describe(name, inlined, path, line, column):
    suffix = " [inlined]" if inlined else ""
    return f"{name}{suffix} at {path}:{line}:{column}"


def frame_path(frame):
    filespec = frame.GetLineEntry().GetFileSpec()
    return os.path.join(filespec.GetDirectory() or "",
                        filespec.GetFilename() or "<no file>")


def main():
    debugger = lldb.SBDebugger.Create()
    debugger.SetAsync(False)
    target = debugger.CreateTarget(EXE)
    assert target.IsValid(), f"cannot create a target for {EXE}"

    break_line, _ = lldb_test_utils.source_pos(INNER_SOURCE, "(x * 3)")
    bp = target.BreakpointCreateByLocation("cu_lib_inner.ml", break_line)
    assert bp.GetNumLocations() > 0, \
        f"breakpoint at cu_lib_inner.ml:{break_line} did not resolve"

    process = target.LaunchSimple(None, None, ".")
    assert process and process.GetState() == lldb.eStateStopped, \
        "process did not stop at the breakpoint"
    stopped = [
        thread for thread in process
        if thread.GetStopReason() == lldb.eStopReasonBreakpoint
    ]
    assert len(stopped) == 1, f"expected 1 stopped thread, got {len(stopped)}"
    thread = stopped[0]

    problems = []
    for i, (name, inlined, path_suffix, (line, column)) in \
            enumerate(expected_frames()):
        frame = thread.GetFrameAtIndex(i)
        entry = frame.GetLineEntry()
        path = frame_path(frame)
        actual = (frame.GetFunctionName() or "<no function>",
                  frame.IsInlined(), path, entry.GetLine(), entry.GetColumn())
        ok = (name in actual[0]
              and (inlined, line, column) == (actual[1], actual[3], actual[4])
              and path.endswith("/" + path_suffix))
        if not ok:
            problems.append(
                f"  frame #{i}:\n"
                f"    expected {describe(name, inlined, '.../' + path_suffix, line, column)}\n"
                f"    got      {describe(*actual)}")
    assert not problems, "backtrace mismatch:\n" + "\n".join(problems)

    process.Kill()


lldb_test_utils.run(main)
