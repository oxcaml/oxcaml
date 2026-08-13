#!/usr/bin/env python3

import argparse
import fcntl
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parent.parent
STATE = ROOT / "_build" / "dev"
PID_FILE = STATE / "watcher.pid"
CHILD_PID_FILE = STATE / "dune.pid"
LEASE_FILE = STATE / "last-used"
TIMEOUT_FILE = STATE / "idle-timeout"
LOG_FILE = STATE / "watcher.log"
LOCK_FILE = STATE / "lock"


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


def start(args):
    if not args.command:
        raise SystemExit("dev watcher: missing watcher command")
    with locked():
        clean_stale_state()
        pid = read_pid(PID_FILE)
        if alive(pid):
            touch_lease(args.idle_timeout)
            return

        touch_lease(args.idle_timeout)
        if LOG_FILE.exists() and LOG_FILE.stat().st_size > 1_000_000:
            LOG_FILE.write_bytes(b"")
        log = LOG_FILE.open("ab", buffering=0)
        command = [
            sys.executable,
            str(Path(__file__).resolve()),
            "supervise",
            "--idle-timeout",
            str(args.idle_timeout),
            "--",
            *args.command,
        ]
        supervisor = subprocess.Popen(
            command,
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
            print(f"dev: watcher started (idle timeout {args.idle_timeout}s)")
            return
        if not alive(supervisor.pid):
            break
        time.sleep(0.05)
    raise SystemExit(f"dev watcher failed to start; see {LOG_FILE}")


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


def stop(_args):
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


def status(_args):
    clean_stale_state()
    pid = read_pid(PID_FILE)
    if not alive(pid):
        print("dev: watcher is stopped")
        return 1
    age = int(time.time() - LEASE_FILE.stat().st_mtime)
    print(f"dev: watcher is running (pid {pid}, idle {age}s)")
    return 0


def wait_ready(args):
    deadline = time.monotonic() + args.timeout
    while time.monotonic() < deadline:
        result = subprocess.run(
            args.command,
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

    touch_parser = commands.add_parser("touch")
    touch_parser.set_defaults(function=lambda _args: touch_lease())

    test_root_parser = commands.add_parser("prepare-test-root")
    test_root_parser.set_defaults(function=prepare_test_root)
    return result


def main():
    args = parser().parse_args()
    has_separator = (
        args.action in {"start", "supervise", "wait-ready"}
        and args.command[:1] == ["--"]
    )
    if has_separator:
        args.command = args.command[1:]
    result = args.function(args)
    return result if isinstance(result, int) else 0


if __name__ == "__main__":
    sys.exit(main())
