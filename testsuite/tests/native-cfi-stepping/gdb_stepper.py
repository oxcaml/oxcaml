import gdb, sys

# This script single-instructions-steps a program in any region between
# calls to 'marker_trace_start' and 'marker_trace_end', verifying that it
# can construct a backtrace at all instructions.
# (It's not checking the content of those backtraces at all, but CFI bugs
# tend to result in stacks that gdb gets stuck trying to unwind, and are
# caught by checking whether you can unwind a couple of frames)

gdb.execute('set pagination off')
try:
    gdb.execute('set debuginfod enabled off')
except:
    pass

# We don't want to single-step the dynamic linker
gdb.execute('set env LD_BIND_NOW=1')

start_trace_bp = gdb.Breakpoint('marker_trace_start')
stop_trace_bp = gdb.Breakpoint('marker_trace_end')

trace_level = 0

def fail_backtrace(i):
    print("LOG_BEGIN")
    print("Backtrace failed:")
    try:
        gdb.execute('bt 4')
    except Exception as e:
        print(f"Cannot produce backtrace: {e}")
    try:
        gdb.execute('disassemble')
    except Exception as e:
        print(f"Cannot disassemble: {e}")
    print("LOG_END")

def stop_handler(event):
    global trace_level
    if isinstance(event, gdb.BreakpointEvent):
        if start_trace_bp in event.breakpoints:
            trace_level += 1
        elif stop_trace_bp in event.breakpoints:
            trace_level -= 1

    # check that we can take a few frames of backtrace
    f = gdb.newest_frame ()
    for i in range(1,4):
        if f is None or (f.name() is None and f.type() != gdb.SIGTRAMP_FRAME):
            fail_backtrace(i)
            break
        f = f.older()

gdb.events.stop.connect(stop_handler)

gdb.execute('run')
while gdb.selected_thread() is not None:
    if trace_level > 0:
        gdb.execute('stepi')
    else:
        gdb.execute('continue')

if trace_level != 0:
    print("LOG_BEGIN")
    print("Program exited unexpectedly")
    print("LOG_END")
