#!/usr/bin/env python3

import argparse
import fcntl
import json
import os
from pathlib import Path
import shlex
import shutil
import signal
import subprocess
import sys
import time


if sys.version_info < (3, 7):
    raise SystemExit(
        "dev watcher: python 3.7 or newer is required "
        f"(running {sys.version.split()[0]} from {sys.executable}); "
        "put a newer python3 first on PATH"
    )


ROOT = Path(__file__).resolve().parent.parent
STATE = ROOT / "_build" / "dev"
PID_FILE = STATE / "watcher.pid"
CHILD_PID_FILE = STATE / "dune.pid"
LEASE_FILE = STATE / "last-used"
TIMEOUT_FILE = STATE / "idle-timeout"
LOG_FILE = STATE / "watcher.log"
LOCK_FILE = STATE / "lock"
WATCHER_COMMAND_FILE = STATE / "watcher-command"
BUILD_LOG_FILE = STATE / "rpc-build.log"

# Dune prints a three-line warning on every rpc-forwarded command, which is
# noise once known. There is no dune flag to suppress it (checked against 3.23),
# so match its body and the bare "Warning:" that introduces it.
FORWARDING_NOTICE = (
    "build request is being forwarded",
    "certain command line arguments may be ignored",
)

# What a dune rpc client says when the watcher's RPC server has gone away.
CONNECTION_FAILURES = ("Connection_dead", "Connection terminated")


def read_pid(path):
    try:
        return int(path.read_text().strip())
    except (FileNotFoundError, ValueError):
        return None


def alive(pid):
    if pid is None:
        return False
    try:
        os.kill(pid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def touch_lease(idle_timeout=None):
    STATE.mkdir(parents=True, exist_ok=True)
    LEASE_FILE.touch()
    if idle_timeout is not None:
        TIMEOUT_FILE.write_text(f"{idle_timeout}\n")


def locked():
    STATE.mkdir(parents=True, exist_ok=True)
    lock = LOCK_FILE.open("a+")
    fcntl.flock(lock, fcntl.LOCK_EX)
    return lock


def clean_stale_state():
    if not alive(read_pid(PID_FILE)):
        PID_FILE.unlink(missing_ok=True)
        CHILD_PID_FILE.unlink(missing_ok=True)


def save_watcher_command(command, idle_timeout):
    STATE.mkdir(parents=True, exist_ok=True)
    WATCHER_COMMAND_FILE.write_text(
        json.dumps({"command": list(command), "idle_timeout": idle_timeout})
        + "\n"
    )


def load_watcher_command():
    try:
        saved = json.loads(WATCHER_COMMAND_FILE.read_text())
    except (FileNotFoundError, ValueError):
        return None
    if not saved.get("command"):
        return None
    return saved["command"], saved.get("idle_timeout", 1800)


def start_watcher(command, idle_timeout):
    if not command:
        raise SystemExit("dev watcher: missing watcher command")
    with locked():
        clean_stale_state()
        save_watcher_command(command, idle_timeout)
        pid = read_pid(PID_FILE)
        if alive(pid):
            touch_lease(idle_timeout)
            return

        touch_lease(idle_timeout)
        if LOG_FILE.exists() and LOG_FILE.stat().st_size > 1_000_000:
            LOG_FILE.write_bytes(b"")
        log = LOG_FILE.open("ab", buffering=0)
        supervisor_command = [
            sys.executable,
            str(Path(__file__).resolve()),
            "supervise",
            "--idle-timeout",
            str(idle_timeout),
            "--",
            *command,
        ]
        supervisor = subprocess.Popen(
            supervisor_command,
            cwd=ROOT,
            stdin=subprocess.DEVNULL,
            stdout=log,
            stderr=subprocess.STDOUT,
            start_new_session=True,
            close_fds=True,
        )
        PID_FILE.write_text(f"{supervisor.pid}\n")

    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        if alive(read_pid(CHILD_PID_FILE)):
            print(f"dev: watcher started (idle timeout {idle_timeout}s)")
            return
        if not alive(supervisor.pid):
            break
        time.sleep(0.05)
    raise SystemExit(f"dev watcher failed to start; see {LOG_FILE}")


def start(args):
    start_watcher(args.command, args.idle_timeout)


def terminate_process_group(child):
    if child.poll() is not None:
        return
    try:
        os.killpg(child.pid, signal.SIGINT)
    except ProcessLookupError:
        return
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline and child.poll() is None:
        time.sleep(0.05)
    if child.poll() is None:
        try:
            os.killpg(child.pid, signal.SIGTERM)
        except ProcessLookupError:
            return


def supervise(args):
    stopping = False

    def request_stop(_signum, _frame):
        nonlocal stopping
        stopping = True

    signal.signal(signal.SIGINT, request_stop)
    signal.signal(signal.SIGTERM, request_stop)

    environment = os.environ.copy()
    environment.pop("MAKEFLAGS", None)
    environment.pop("MFLAGS", None)
    child = subprocess.Popen(
        args.command,
        cwd=ROOT,
        stdin=subprocess.DEVNULL,
        start_new_session=True,
        env=environment,
    )
    CHILD_PID_FILE.write_text(f"{child.pid}\n")

    try:
        while child.poll() is None:
            try:
                idle_for = time.time() - LEASE_FILE.stat().st_mtime
            except FileNotFoundError:
                idle_for = args.idle_timeout
            try:
                idle_timeout = int(TIMEOUT_FILE.read_text().strip())
            except (FileNotFoundError, ValueError):
                idle_timeout = args.idle_timeout
            if stopping or idle_for >= idle_timeout:
                terminate_process_group(child)
                break
            time.sleep(min(1, idle_timeout))
        return child.wait()
    finally:
        with locked():
            if read_pid(PID_FILE) == os.getpid():
                PID_FILE.unlink(missing_ok=True)
                CHILD_PID_FILE.unlink(missing_ok=True)


def stop_watcher():
    with locked():
        clean_stale_state()
        pid = read_pid(PID_FILE)
        if pid is None:
            return
        try:
            os.kill(pid, signal.SIGTERM)
        except ProcessLookupError:
            clean_stale_state()
            return

    deadline = time.monotonic() + 7
    while time.monotonic() < deadline and alive(pid):
        time.sleep(0.05)
    if alive(pid):
        raise SystemExit(f"dev watcher {pid} did not stop")
    print("dev: watcher stopped")


def stop(_args):
    stop_watcher()


def status(_args):
    clean_stale_state()
    pid = read_pid(PID_FILE)
    if not alive(pid):
        print("dev: watcher is stopped")
        return 1
    age = int(time.time() - LEASE_FILE.stat().st_mtime)
    print(f"dev: watcher is running (pid {pid}, idle {age}s)")
    return 0


def await_ready(command, timeout):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        result = subprocess.run(
            command,
            cwd=ROOT,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        if result.returncode == 0:
            return 0
        if not alive(read_pid(PID_FILE)):
            raise SystemExit(f"dev watcher exited; see {LOG_FILE}")
        time.sleep(0.1)
    raise SystemExit(f"dev watcher did not become ready; see {LOG_FILE}")


def wait_ready(args):
    return await_ready(args.command, args.timeout)


def announce(message):
    print(f"dev: {message}", flush=True)


def run_with_heartbeat(command, timeout, heartbeat):
    """Run [command], capturing its combined output into BUILD_LOG_FILE while
    printing a heartbeat so a long build is distinguishable from a wedged one.
    Returns (exit status, output), with a status of None on timeout."""
    BUILD_LOG_FILE.parent.mkdir(parents=True, exist_ok=True)
    with BUILD_LOG_FILE.open("wb") as sink:
        child = subprocess.Popen(
            command,
            cwd=ROOT,
            stdin=subprocess.DEVNULL,
            stdout=sink,
            stderr=subprocess.STDOUT,
            start_new_session=True,
        )
        start_time = time.monotonic()
        next_heartbeat = start_time + heartbeat
        timed_out = False
        while child.poll() is None:
            now = time.monotonic()
            if timeout and now - start_time >= timeout:
                announce(f"the build exceeded {timeout}s; stopping it")
                terminate_process_group(child)
                child.wait()
                timed_out = True
                break
            if heartbeat and now >= next_heartbeat:
                announce(
                    f"still building ({int(now - start_time)}s elapsed; "
                    "progress: make dev-log)"
                )
                next_heartbeat = now + heartbeat
            time.sleep(0.2)
    output = BUILD_LOG_FILE.read_text(errors="replace")
    return (None if timed_out else child.returncode), output


def emit(output):
    lines = output.splitlines()
    noise = [
        any(fragment in line for fragment in FORWARDING_NOTICE)
        for line in lines
    ]
    for index, line in enumerate(lines):
        introduces_noise = (
            line.strip() == "Warning:"
            and index + 1 < len(lines)
            and noise[index + 1]
        )
        if introduces_noise:
            noise[index] = True
    for line, is_noise in zip(lines, noise):
        if not is_noise:
            print(line, flush=True)


def filter_notices(_args):
    emit(sys.stdin.read())


def lost_connection(output):
    return any(failure in output for failure in CONNECTION_FAILURES)


def restart_watcher(ping, ready_timeout):
    saved = load_watcher_command()
    if saved is None:
        announce("no saved watcher command, so the watcher cannot be restarted")
        return False
    command, idle_timeout = saved
    try:
        stop_watcher()
        start_watcher(command, idle_timeout)
        if ping:
            await_ready(ping, ready_timeout)
    except SystemExit as failure:
        announce(f"restarting the watcher failed: {failure}")
        return False
    return True


def attempt_rpc_build(args):
    status, output = run_with_heartbeat(
        args.command, args.timeout, args.heartbeat
    )
    if status is None:
        return None, output, f"timed out after {args.timeout}s"
    if lost_connection(output):
        return None, output, "lost its connection to the watcher"
    return status, output, None


def build(args):
    """Build through the watcher's RPC, recovering from a wedged watcher.

    The observed failure is an rpc client that waits forever against a watcher
    that is alive and answers pings but never starts the build. So: bound the
    wait, bounce the watcher and retry exactly once, and if that also fails
    build directly. Retrying without a bound would just be a new silent hang.
    """
    touch_lease()
    if args.ping:
        await_ready(args.ping, args.ready_timeout)
    announce("building via the watcher (progress: make dev-log)")
    status, output, failure = attempt_rpc_build(args)

    if failure is not None:
        announce(f"the build {failure}; restarting the watcher and retrying")
        emit(output)
        if restart_watcher(args.ping, args.ready_timeout):
            status, output, failure = attempt_rpc_build(args)
        else:
            failure = "could not restart the watcher"
        if failure is not None:
            announce(f"the build {failure}; building directly instead")
            emit(output)
            return build_directly(args.fallback)

    emit(output)
    if status == 0 and "Success" in output.splitlines():
        return 0
    if args.diagnostics:
        subprocess.run(args.diagnostics, cwd=ROOT)
    return 1


def build_directly(fallback):
    if not fallback:
        announce("no direct build command was given")
        return 1
    announce("building directly (no watcher, no rpc; slower)")
    return subprocess.run(fallback, cwd=ROOT).returncode


# Where a test's fresh output can end up. dev-test uses the dev root; dev-test-all
# uses _runtest; and the promote path for a whole directory runs ocamltest with
# OCAMLTESTDIR under the test's own directory, which the dev root symlinks back
# into the source tree.
ARTIFACT_ROOTS = ("_build/dev/runtest/testsuite", "_runtest/testsuite", "testsuite")


def newest(paths):
    return max(paths, key=lambda path: path.stat().st_mtime)


def find_artifacts(patterns):
    for pattern in patterns:
        found = [
            path
            for root in ARTIFACT_ROOTS
            if (ROOT / root).is_dir()
            for path in (ROOT / root).rglob(pattern)
            if path.is_file()
        ]
        if found:
            return newest(found)
    return None


def diff(args):
    """Show a test's newest fresh output against what it is compared to.

    Expect tests are the subtle case: the -principal pass writes
    <test>.corrected.corrected, which supersedes <test>.corrected, so promoting
    the latter by hand drops the principal updates silently.
    """
    source = ROOT / "testsuite" / args.test
    stem = source.name.rsplit(".", 1)[0]

    corrected = find_artifacts(
        [f"{source.name}.corrected.corrected", f"{source.name}.corrected"]
    )
    if corrected is not None:
        announce(f"corrected output {corrected.relative_to(ROOT)}")
        announce("promote with `make dev-promote`, never by copying this file")
        return show_diff(source, corrected)

    output = find_artifacts([f"{stem}.output", f"{stem}.result"])
    if output is None:
        announce(f"no fresh output for {source.relative_to(ROOT)}")
        announce("run `make dev-test TEST=...` first; note that prepare-test-root")
        announce("discards the previous run's artifacts")
        return 1

    announce(f"program output {output.relative_to(ROOT)}")
    reference = source.parent / f"{stem}.reference"
    if not reference.is_file():
        announce(f"no reference {reference.relative_to(ROOT)} yet; the output is:")
        print(output.read_text(errors="replace"), end="")
        return 0
    return show_diff(reference, output)


def show_diff(reference, output):
    subprocess.run(["diff", "-u", str(reference), str(output)], cwd=ROOT)
    return 0


def link(source, destination):
    destination.symlink_to(
        source.resolve(), target_is_directory=source.is_dir()
    )


def prepare_test_root_locked():
    source_root = ROOT / "_runtest"
    runtime_stdlib = (
        ROOT
        / "_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib"
    )
    if not source_root.is_dir() or not runtime_stdlib.is_dir():
        raise SystemExit(
            "dev: run `make install` once before using development tests"
        )

    destination = STATE / "runtest"
    temporary = STATE / "runtest.new"
    shutil.rmtree(temporary, ignore_errors=True)
    temporary.mkdir(parents=True)

    overridden = {
        "ocamlc",
        "ocamlc.byte",
        "ocamlc.opt",
        "ocamlopt",
        "ocamlopt.byte",
        "ocamlopt.opt",
        "ocamlrun",
        "ocamlrund",
        "ocamlruni",
        "ocamltest",
        "runtime",
        "stdlib",
        "testsuite",
    }
    for entry in source_root.iterdir():
        if entry.name not in overridden:
            link(entry, temporary / entry.name)

    link(ROOT / "_build/dev-dune/default/main.bc", temporary / "ocamlc.byte")
    (temporary / "ocamlc").symlink_to("ocamlc.byte")
    link(
        ROOT / "_build/dev-dune/default/main_native.exe",
        temporary / "ocamlc.opt",
    )
    link(
        ROOT / "_build/dev-dune/default/boot_ocamlopt.exe",
        temporary / "ocamlopt.opt",
    )
    # ocamltest resolves its "ocamlopt.byte" action to $srcdir/ocamlopt
    # (ocamltest/ocaml_files.ml), so without these the whole flavour fails with
    # "cannot find file .../ocamlopt" rather than running against the dev build.
    link(
        ROOT / "_build/dev-dune/default/boot_ocamlopt.exe",
        temporary / "ocamlopt.byte",
    )
    (temporary / "ocamlopt").symlink_to("ocamlopt.byte")
    for name in ("ocamlrun", "ocamlrund", "ocamlruni"):
        link(
            ROOT / f"_build/runtime_stdlib_install/bin/{name}",
            temporary / name,
        )
    stdlib = temporary / "stdlib"
    stdlib.mkdir()
    for entry in runtime_stdlib.iterdir():
        if entry.name != "stublibs":
            link(entry, stdlib / entry.name)

    stublibs = stdlib / "stublibs"
    stublibs.mkdir()
    dev_stubs = {
        stub.name: stub
        for stub in (ROOT / "_build/dev-dune/default").rglob("dll*.so")
    }
    for stub in (source_root / "stdlib/stublibs").iterdir():
        if stub.name not in dev_stubs:
            link(stub, stublibs / stub.name)
    for name, stub in dev_stubs.items():
        link(stub, stublibs / name)

    runtime = temporary / "runtime"
    runtime.mkdir()
    for entry in (source_root / "runtime").iterdir():
        if entry.name not in {
            "caml", "ocamlrun", "ocamlrund", "ocamlruni", "threads.h"
        }:
            link(entry, runtime / entry.name)
    (runtime / "caml").symlink_to("../stdlib/caml")
    link(ROOT / "runtime/caml/threads.h", runtime / "threads.h")
    for name in ("ocamlrun", "ocamlrund", "ocamlruni"):
        (runtime / name).symlink_to(f"../{name}")

    ocamltest = temporary / "ocamltest"
    ocamltest.mkdir()
    link(
        ROOT / "_build/dev-dune/default/ocamltest/ocamltest.native",
        ocamltest / "ocamltest",
    )

    source_testsuite = source_root / "testsuite"
    testsuite = temporary / "testsuite"
    testsuite.mkdir()
    for entry in source_testsuite.iterdir():
        if entry.name not in {"tests", "tools"}:
            link(entry, testsuite / entry.name)

    source_tools = source_testsuite / "tools"
    tools = testsuite / "tools"
    tools.mkdir()
    for entry in source_tools.iterdir():
        if entry.name not in {"expect", "expectnat"}:
            link(entry, tools / entry.name)
    for name in ("expect", "expectnat"):
        executable = ROOT / f"_build/main/oxcaml/testsuite/tools/{name}.exe"
        if executable.exists():
            link(executable, tools / name)

    tests = testsuite / "tests"
    tests.mkdir()
    replacements = {"asmcomp", "asmgen", "lib-extensions"}
    for entry in (ROOT / "testsuite/tests").iterdir():
        if entry.name not in replacements:
            link(entry, tests / entry.name)
    for name in replacements:
        link(ROOT / "oxcaml/testsuite/tests" / name, tests / name)

    old = STATE / "runtest.old"
    shutil.rmtree(old, ignore_errors=True)
    if destination.exists():
        destination.rename(old)
    temporary.rename(destination)
    shutil.rmtree(old, ignore_errors=True)


def prepare_test_root(_args):
    with locked():
        prepare_test_root_locked()


def parser():
    result = argparse.ArgumentParser()
    commands = result.add_subparsers(dest="action", required=True)

    start_parser = commands.add_parser("start")
    start_parser.add_argument("--idle-timeout", type=int, required=True)
    start_parser.add_argument("command", nargs=argparse.REMAINDER)
    start_parser.set_defaults(function=start)

    supervise_parser = commands.add_parser("supervise")
    supervise_parser.add_argument("--idle-timeout", type=int, required=True)
    supervise_parser.add_argument("command", nargs=argparse.REMAINDER)
    supervise_parser.set_defaults(function=supervise)

    stop_parser = commands.add_parser("stop")
    stop_parser.set_defaults(function=stop)

    status_parser = commands.add_parser("status")
    status_parser.set_defaults(function=status)

    ready_parser = commands.add_parser("wait-ready")
    ready_parser.add_argument("--timeout", type=int, default=300)
    ready_parser.add_argument("command", nargs=argparse.REMAINDER)
    ready_parser.set_defaults(function=wait_ready)

    build_parser = commands.add_parser("build")
    build_parser.add_argument("--timeout", type=int, default=1800)
    build_parser.add_argument("--heartbeat", type=int, default=30)
    build_parser.add_argument("--ready-timeout", type=int, default=300)
    build_parser.add_argument("--ping", type=shlex.split, default=[])
    build_parser.add_argument("--fallback", type=shlex.split, default=[])
    build_parser.add_argument("--diagnostics", type=shlex.split, default=[])
    build_parser.add_argument("command", nargs=argparse.REMAINDER)
    build_parser.set_defaults(function=build)

    touch_parser = commands.add_parser("touch")
    touch_parser.set_defaults(function=lambda _args: touch_lease())

    filter_parser = commands.add_parser("filter-notices")
    filter_parser.set_defaults(function=filter_notices)

    diff_parser = commands.add_parser("diff")
    diff_parser.add_argument("--test", required=True)
    diff_parser.set_defaults(function=diff)

    test_root_parser = commands.add_parser("prepare-test-root")
    test_root_parser.set_defaults(function=prepare_test_root)
    return result


def main():
    args = parser().parse_args()
    has_separator = (
        args.action in {"start", "supervise", "wait-ready", "build"}
        and args.command[:1] == ["--"]
    )
    if has_separator:
        args.command = args.command[1:]
    result = args.function(args)
    return result if isinstance(result, int) else 0


if __name__ == "__main__":
    sys.exit(main())
