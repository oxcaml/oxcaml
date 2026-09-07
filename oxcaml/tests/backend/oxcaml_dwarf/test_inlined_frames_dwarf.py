"""Check DW_AT_call_file/line/column attribution of inlined frames.

A three-deep [@inline always] chain func1 -> func2 -> func3 is inlined into
f_caller. In a backtrace from inside func3's body, each caller frame must be
shown at the position of the corresponding call, not at a position inside
the callee's own body. A regression that emits a frame's own position
instead of its call site (as was once the case) shifts every frame's
location one level too deep.
"""

import lldb
import lldb_test_utils

EXE = "./test_inlined_frames_dwarf.exe"
SOURCE = "test_inlined_frames_dwarf.ml"

INLINED = True
NOT_INLINED = False


def source_pos(fragment):
    return lldb_test_utils.source_pos(SOURCE, fragment)


def expected_frames():
    """Innermost first: (function name fragment, inlined?, (line, column))."""
    return [
        # The innermost frame is at its own position inside func3's body.
        ("func3", INLINED, source_pos("(x * 3)")),
        # Each caller frame is at the position of its call.
        ("func2", INLINED, source_pos("func3 (x + 2)")),
        ("func1", INLINED, source_pos("func2 (x lxor 1)")),
        ("f_caller", NOT_INLINED, source_pos("func1 (Sys.opaque_identity x)")),
    ]


def describe(name, inlined, line, column):
    suffix = " [inlined]" if inlined else ""
    return f"{name}{suffix} at {line}:{column}"


def main():
    debugger = lldb.SBDebugger.Create()
    debugger.SetAsync(False)
    target = debugger.CreateTarget(EXE)
    assert target.IsValid(), f"cannot create a target for {EXE}"

    break_line, _ = source_pos("(x * 3)")
    bp = target.BreakpointCreateByLocation(SOURCE, break_line)
    assert bp.GetNumLocations() > 0, \
        f"breakpoint at {SOURCE}:{break_line} did not resolve"

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
    for i, (name, inlined, (line, column)) in enumerate(expected_frames()):
        frame = thread.GetFrameAtIndex(i)
        entry = frame.GetLineEntry()
        actual = (frame.GetFunctionName() or "<no function>",
                  frame.IsInlined(), entry.GetLine(), entry.GetColumn())
        if not (name in actual[0] and (inlined, line, column) == actual[1:]):
            problems.append(
                f"  frame #{i}:\n"
                f"    expected {describe(name, inlined, line, column)}\n"
                f"    got      {describe(*actual)}")
    assert not problems, "backtrace mismatch:\n" + "\n".join(problems)

    process.Kill()


lldb_test_utils.run(main)
