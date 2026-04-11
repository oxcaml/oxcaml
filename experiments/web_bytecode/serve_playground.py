#!/usr/bin/env python3

import argparse
import json
import os
import subprocess
import sys
from http import HTTPStatus
from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler
from pathlib import Path


EXPERIMENT_DIR = Path(__file__).resolve().parent
REPO_ROOT = EXPERIMENT_DIR.parent.parent
WEB_BUILD_DIR = REPO_ROOT / "_build" / "web" / "experiments" / "web_bytecode"
OCAMLRUN = REPO_ROOT / "_install" / "bin" / "ocamlrun"
CHECK_CLI = WEB_BUILD_DIR / "web_bytecode_check_cli.bc"
RUN_CLI = WEB_BUILD_DIR / "web_bytecode_cli.bc"


def tool_env() -> dict[str, str]:
  env = os.environ.copy()
  env["PATH"] = f"{REPO_ROOT / '_install' / 'bin'}:{env.get('PATH', '')}"
  env["OCAMLLIB"] = str(REPO_ROOT / "_install" / "lib" / "ocaml")
  env["CAML_LD_LIBRARY_PATH"] = str(REPO_ROOT / "_install" / "lib" / "ocaml" / "stublibs")
  env["RUNTIME_DIR"] = "runtime"
  return env


def ensure_targets() -> None:
  subprocess.run(
      [
          "dune",
          "build",
          "--root=.",
          "--workspace=duneconf/web.ws",
          "experiments/web_bytecode/web_bytecode_cli.bc",
          "experiments/web_bytecode/web_bytecode_check_cli.bc",
      ],
      cwd=REPO_ROOT,
      env=tool_env(),
      check=True,
  )


def run_compiler(mode: str, filename: str, source: str) -> str:
  if mode == "check":
    command = [str(OCAMLRUN), str(CHECK_CLI), filename]
  elif mode == "run":
    command = [str(OCAMLRUN), str(RUN_CLI), "run", "-", filename]
  else:
    raise ValueError(f"unsupported mode: {mode}")
  completed = subprocess.run(
      command,
      cwd=REPO_ROOT,
      env=tool_env(),
      input=source,
      text=True,
      capture_output=True,
      check=False,
  )
  if completed.returncode != 0:
    message = completed.stderr or completed.stdout or f"{mode} failed with exit code {completed.returncode}"
    raise RuntimeError(message.strip())
  return completed.stdout


class PlaygroundHandler(SimpleHTTPRequestHandler):
  def __init__(self, *args, **kwargs):
    super().__init__(*args, directory=str(EXPERIMENT_DIR), **kwargs)

  def end_headers(self) -> None:
    self.send_header("Cache-Control", "no-store")
    super().end_headers()

  def do_POST(self) -> None:
    if self.path not in {"/api/check", "/api/run"}:
      self.send_error(HTTPStatus.NOT_FOUND, "Unknown endpoint")
      return
    try:
      length = int(self.headers.get("Content-Length", "0"))
      raw_body = self.rfile.read(length)
      payload = json.loads(raw_body.decode("utf-8"))
      filename = payload.get("filename", "snippet.ml")
      source = payload.get("source", "")
      output = run_compiler("check" if self.path.endswith("/check") else "run", filename, source)
      body = json.dumps({"output": output}).encode("utf-8")
      self.send_response(HTTPStatus.OK)
    except Exception as exc:  # noqa: BLE001
      body = json.dumps({"error": str(exc)}).encode("utf-8")
      self.send_response(HTTPStatus.INTERNAL_SERVER_ERROR)
    self.send_header("Content-Type", "application/json; charset=utf-8")
    self.send_header("Content-Length", str(len(body)))
    self.end_headers()
    self.wfile.write(body)

  def do_GET(self) -> None:
    if self.path == "/":
      self.path = "/index.html"
    super().do_GET()


def main() -> int:
  parser = argparse.ArgumentParser(description="Serve the OxCaml playground with a real native backend.")
  parser.add_argument("--host", default="127.0.0.1")
  parser.add_argument("--port", type=int, default=8000)
  parser.add_argument("--skip-build", action="store_true")
  args = parser.parse_args()

  if not args.skip_build:
    ensure_targets()

  server = ThreadingHTTPServer((args.host, args.port), PlaygroundHandler)
  print(f"Serving OxCaml playground at http://{args.host}:{args.port}", flush=True)
  try:
    server.serve_forever()
  except KeyboardInterrupt:
    pass
  finally:
    server.server_close()
  return 0


if __name__ == "__main__":
  sys.exit(main())
