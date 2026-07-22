"""LLDB helper for the OCaml/C boundary DWARF test.

Defines a `frames` command that prints only the backtrace frames whose
function (or symbol) name contains one of the given names, in stack order,
using LLDB's native frame formatting.
"""

import lldb

_default_frame_format = ""


def _get_frame_format(debugger):
    vals = lldb.SBDebugger.GetInternalVariableValue(
        "frame-format", debugger.GetInstanceName())
    return vals.GetStringAtIndex(0) if vals and vals.GetSize() else ""


def _set_frame_format(debugger, value):
    lldb.SBDebugger.SetInternalVariable(
        "frame-format", value, debugger.GetInstanceName())


def _frame_name(frame):
    name = frame.GetFunctionName()
    if name:
        return name
    symbol = frame.GetSymbol()
    if symbol and symbol.GetName():
        return symbol.GetName()
    return frame.GetDisplayFunctionName() or ""


def frames(debugger, command, result, internal_dict):
    # Restore the real frame format so GetDescription produces the full frame
    # line (with formatted arguments), then blank it again so the next
    # breakpoint stop prints no frame line of its own.
    _set_frame_format(debugger, _default_frame_format)
    wanted = command.split()
    thread = debugger.GetSelectedTarget().GetProcess().GetSelectedThread()
    for frame in thread.frames:
        name = _frame_name(frame)
        if any(keyword in name for keyword in wanted):
            stream = lldb.SBStream()
            frame.GetDescription(stream)
            result.AppendMessage("    " + stream.GetData().rstrip("\n"))
    _set_frame_format(debugger, "")


def __lldb_init_module(debugger, internal_dict):
    global _default_frame_format
    _default_frame_format = _get_frame_format(debugger)
    _set_frame_format(debugger, "")
    debugger.HandleCommand("command script add -f frames.frames frames")
