"""Check experimental-feature metadata (Python 3.6+, standard library only)."""

import argparse
import html
import io
from pathlib import Path, PurePosixPath
import re
import subprocess
import sys
from typing import (Dict, Iterable, Iterator, List, NamedTuple, Optional,
                    Pattern, Tuple)


TOKEN = r"[a-z0-9][a-z0-9.-]*"
LEVELS = ("ai-reviewed", "human-reviewed")
RESERVED_FLAGS = ("all", "universe", "optimizations")
FIELDS = ("OxCaml-Written-By", "OxCaml-Review-Status")
MODELS_PATH = "scrutiny/models"
STATUS_PATH = "scrutiny/STATUS.md"


# How to find the comments and strings of one language.
class Syntax(NamedTuple):
    lexeme: Pattern  # comment delimiters and string openers
    string_end: Pattern  # an escape sequence, or the closing quote


OCAML = Syntax(
    # Skip character literals before looking for quotes or comment delimiters.
    re.compile(
        r"'(?:\\.|[^'\\\n])'|\(\*|\*\)|"
        r"\{(?:%%?[^\W\d][\w'.]*[ \t\f]*)?([^\W\d]*)\||\""),
    re.compile(r'\\.|"', re.DOTALL))
# Rocq has no character literals or quoted strings ({x|P} and {| ... |} are
# ordinary syntax) and escapes a quote by doubling it, not with a backslash.
ROCQ = Syntax(re.compile(r'\(\*|\*\)|"'), re.compile(r'""|"'))
SYNTAXES = {".ml": OCAML, ".mli": OCAML, ".v": ROCQ}
# ocamlformat leaves comments opened this way verbatim; with wrap-comments it
# would otherwise join the header fields onto one line.
HEADER_OPEN = "(*="
HEADER_START = re.compile(
    r"^[ \t]*(?:\(\*=?[ \t]*)?OxCaml-"
    r"(?:Written-By\b|Review-Status\b|[A-Za-z-]+[ \t]*:)", re.MULTILINE)


class CheckError(Exception):
    pass


class SourceToken(NamedTuple):
    kind: str
    line: int
    text: str


class HeaderField(NamedTuple):
    line: int
    values: Tuple[str, ...]


class Registration(NamedTuple):
    path: str
    flag: str
    level: str


class ReviewedFile(NamedTuple):
    path: str
    fields: Dict[str, HeaderField]


def error(path: str, line: int, message: str) -> CheckError:
    return CheckError("{}:{}: {}".format(path, line, message))


def entries(text: str) -> Iterator[Tuple[int, List[str]]]:
    for line, raw in enumerate(text.splitlines(), 1):
        words = raw.split("#", 1)[0].split()
        if words:
            yield line, words


def parse_models(text: str) -> Dict[str, bool]:
    """Map each writing/reviewing entry to whether it is retired."""
    models = {}
    for line, words in entries(text):
        if (len(words) not in (1, 2)
                or (len(words) == 2 and words[1] != "retired")
                or not re.fullmatch(TOKEN + r"(?::" + TOKEN + r")?", words[0])):
            raise error(MODELS_PATH, line,
                        "expected <model> or <model>:<effort>, "
                        "optionally followed by retired")
        name = words[0]
        if name in models:
            raise error(MODELS_PATH, line, "duplicate entry " + name)
        models[name] = len(words) == 2
    return models


def parse_registration(path: str, text: str) -> Registration:
    values = {}
    for line, words in entries(text):
        key = words[0]
        if key not in ("flag", "level"):
            raise error(path, line, "unknown key " + key)
        if key in values:
            raise error(path, line, "duplicate key " + key)
        if len(words) != 2 or not re.fullmatch(TOKEN, words[1]):
            raise error(path, line, "expected {} <token>".format(key))
        value = words[1]
        if key == "level" and value not in LEVELS:
            raise error(path, line, "unknown review level " + value)
        if key == "flag" and value in RESERVED_FLAGS:
            raise error(path, line, "reserved flag name " + value)
        values[key] = value
    for key in ("flag", "level"):
        if key not in values:
            raise error(path, 1, "missing " + key)
    return Registration(path, values["flag"], values["level"])


def source_tokens(lines: Iterable[str],
                  syntax: Syntax) -> Iterator[SourceToken]:
    """Read comments and strings lazily, respecting nested comments.

    Strings and character literals, as the syntax defines them, must not
    manufacture header comments. This is a lexical scan, not an OCaml/Rocq
    syntax check: compiler test inputs may deliberately contain incomplete
    strings or comments.
    """
    depth = 0
    quote = None
    parts = []
    start_line = 1
    for number, line in enumerate(lines, 1):
        pos = 0
        while pos < len(line):
            if quote is not None:
                if quote == '"':
                    match = syntax.string_end.search(line, pos)
                    end = match.end() if match else len(line)
                    closed = match is not None and match.group() == '"'
                else:
                    index = line.find(quote, pos)
                    closed = index >= 0
                    end = index + len(quote) if closed else len(line)
                parts.append(line[pos:end])
                pos = end
                if closed:
                    quote = None
                    if depth == 0:
                        yield SourceToken("string", start_line, "".join(parts))
                        parts = []
                continue
            match = syntax.lexeme.search(line, pos)
            if match is None:
                if depth:
                    parts.append(line[pos:])
                break
            token = match.group()
            if depth:
                parts.append(line[pos:match.end()])
            elif token == "(*" or token == '"' or token.startswith("{"):
                start_line = number
                parts = [token]
            pos = match.end()
            if token == "(*":
                depth += 1
            elif token == "*)" and depth:
                depth -= 1
                if depth == 0:
                    yield SourceToken("comment", start_line, "".join(parts))
                    parts = []
            elif token == '"':
                quote = '"'
            elif token.startswith("{"):
                quote = "|" + match.group(1) + "}"
    if depth:
        yield SourceToken("unfinished-comment", start_line, "".join(parts))


def first_stanza(lines: Iterable[str],
                 syntax: Syntax) -> Optional[SourceToken]:
    for token in source_tokens(lines, syntax):
        if token.kind != "string" and HEADER_START.search(token.text):
            return token
    return None


def parse_header(path: str, stanza: SourceToken) -> Dict[str, HeaderField]:
    if stanza.kind == "unfinished-comment":
        raise error(path, stanza.line, "unterminated OxCaml header comment")
    if not stanza.text.startswith(HEADER_OPEN):
        raise error(path, stanza.line,
                    "OxCaml header comment must open with " + HEADER_OPEN)
    fields = {}
    body_lines = stanza.text[len(HEADER_OPEN):-2].splitlines()
    for offset, body in enumerate(body_lines):
        if not HEADER_START.match(body):
            continue
        line = stanza.line + offset
        match = re.fullmatch(r"\s*(OxCaml-[A-Za-z-]+):\s*(.*?)\s*", body)
        if match is None:
            raise error(path, line, "malformed OxCaml header line")
        key, value = match.groups()
        if key not in FIELDS:
            raise error(path, line, "unknown header field " + key)
        if key in fields:
            raise error(path, line, "duplicate header field " + key)
        values = tuple(item.strip() for item in value.split(","))
        if any(not item for item in values):
            raise error(path, line, key + " must be a nonempty set")
        if len(set(values)) != len(values):
            raise error(path, line, "duplicate value in " + key)
        pattern = "ai:" + TOKEN
        if key == FIELDS[1]:
            pattern += ":" + TOKEN
        for item in values:
            if item != "human" and not re.fullmatch(pattern, item):
                raise error(path, line,
                            "invalid {} value: {}".format(key, item))
        fields[key] = HeaderField(line, values)
    for key in FIELDS:
        if key not in fields:
            raise error(path, stanza.line, "missing " + key)
    if not any(item.startswith("ai:") for item in fields[FIELDS[1]].values):
        raise error(path, fields[FIELDS[1]].line,
                    "OxCaml-Review-Status requires at least one ai: entry")
    return fields


def check_models(path: str, fields: Dict[str, HeaderField],
                 models: Dict[str, bool],
                 old_fields: Optional[Dict[str, HeaderField]]) -> List[str]:
    """Check a header's entries against the allowlist.

    A retired entry is accepted only where old_fields, the header at the
    base, has it in the same field. None means there is no base to compare
    with, so retirement is not checked.
    """
    errors = []
    for key, field in fields.items():
        for value in field.values:
            if value == "human":
                continue
            model = value[3:]
            if model not in models:
                errors.append(str(error(path, field.line,
                                        "unapproved model entry " + model)))
            elif (models[model] and old_fields is not None
                  and (key not in old_fields
                       or value not in old_fields[key].values)):
                errors.append(str(error(
                    path, field.line, "header adds retired entry " + model)))
    return errors


def git(root: Path, *args: str) -> bytes:
    result = subprocess.run(
        ["git", "--no-pager"] + list(args), cwd=str(root),
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode:
        raise CheckError("git {}: {}".format(
            " ".join(args), result.stderr.decode("utf-8", "replace").strip()))
    return result.stdout


def git_paths(output: bytes) -> List[str]:
    return [item.decode("utf-8", "surrogateescape")
            for item in output.split(b"\0") if item]


def old_paths(root: Path, base: str) -> Dict[str, Optional[str]]:
    """Map additions to None and renames to their base paths."""
    fields = iter(git_paths(git(
        root, "diff", "--no-ext-diff", "--no-textconv", "--name-status",
        "-z", "--find-renames", base, "--")))
    paths = {}
    for status in fields:
        path = next(fields)
        if status.startswith("R"):
            paths[next(fields)] = path
        elif status == "A":
            paths[path] = None
    return paths


def read_metadata(root: Path, path: str) -> str:
    try:
        return (root / path).read_text(encoding="utf-8")
    except (OSError, UnicodeError) as exc:
        raise error(path, 1, str(exc))


def read_stanza(root: Path, path: str,
                syntax: Syntax) -> Optional[SourceToken]:
    try:
        # Compiler test inputs need not be valid UTF-8.
        with (root / path).open(
                encoding="utf-8", errors="surrogateescape") as f:
            return first_stanza(f, syntax)
    except OSError as exc:
        raise error(path, 1, str(exc))


def old_header(root: Path, base: str, path: str,
               syntax: Syntax) -> Dict[str, HeaderField]:
    """Read a header at the base; a missing or invalid one has no fields."""
    content = git(root, "show", base + ":" + path)
    stanza = first_stanza(io.StringIO(
        content.decode("utf-8", "surrogateescape")), syntax)
    if stanza is None:
        return {}
    try:
        return parse_header(path, stanza)
    except CheckError:
        return {}


def driver_options(root: Path, paths: Iterable[str]) -> Dict[str, str]:
    """Find experimental option literals without matching comments/help text."""
    options = {}
    pattern = re.compile(r'"(-(no-)?experimental-' + TOKEN + r')"')
    for path in paths:
        if not path.startswith("driver/") or not path.endswith(".ml"):
            continue
        with (root / path).open(
                encoding="utf-8", errors="surrogateescape") as f:
            for token in source_tokens(f, OCAML):
                if token.kind == "string":
                    match = pattern.fullmatch(token.text)
                    if match:
                        options[match.group(1)] = "{}:{}".format(
                            path, token.line)
    return options


def markdown_cell(text: str) -> str:
    return (html.escape(text).replace("|", "&#124;")
            .replace("\n", "&#10;").replace("\r", "&#13;")
            .replace("`", "&#96;").replace("\\", "&#92;"))


def render_status(registrations: Iterable[Registration],
                  files: Dict[str, List[ReviewedFile]]) -> str:
    lines = ["# Experimental feature review status", "",
             "Generated by `scrutiny/check.py`; do not edit by hand.", ""]
    ordered = sorted(registrations, key=lambda registration: registration.flag)
    if not ordered:
        lines.append("No experimental features are registered.")
    else:
        lines.extend([
            "| Feature | Directory | Level | File | Written by | Reviewed by |",
            "|---|---|---|---|---|---|",
        ])
        for registration in ordered:
            directory = str(PurePosixPath(registration.path).parent)
            for source in sorted(files[directory], key=lambda item: item.path):
                cells = [registration.flag, directory, registration.level,
                         source.path]
                cells.extend(
                    ", ".join(sorted(source.fields[key].values))
                    for key in FIELDS)
                row = " | ".join(map(markdown_cell, cells))
                lines.append("| " + row + " |")
    return "\n".join(lines) + "\n"


def check_repository(root: Path, *, base: Optional[str] = None,
                     update: bool = False) -> List[str]:
    paths = sorted(set(git_paths(git(root, "ls-files", "-z"))))
    if git(root, "ls-files", "--unmerged", "-z"):
        raise CheckError("resolve Git index conflicts before checking scrutiny")
    previous_paths = {}
    if base is not None:
        if not base or base.startswith("-"):
            raise CheckError("--base must be a nonempty revision "
                             "not starting with '-'")
        base = git(root, "rev-parse", "--verify",
                   base + "^{commit}").decode("ascii").strip()
        previous_paths = old_paths(root, base)
    errors = []
    try:
        models = parse_models(read_metadata(root, MODELS_PATH))
    except CheckError as exc:
        errors.append(str(exc))
        models = None
    registrations = {}
    flags = {}
    for path in paths:
        if PurePosixPath(path).name != "SCRUTINY":
            continue
        directory = str(PurePosixPath(path).parent)
        registrations[directory] = None
        try:
            registration = parse_registration(path, read_metadata(root, path))
            registrations[directory] = registration
            if registration.flag in flags:
                errors.append(str(error(
                    path, 1, "duplicate flag {}; also in {}".format(
                        registration.flag, flags[registration.flag]))))
            flags[registration.flag] = path
        except CheckError as exc:
            errors.append(str(exc))
    options = driver_options(root, paths)
    for directory, registration in registrations.items():
        for parent in PurePosixPath(directory).parents:
            if str(parent) in registrations:
                errors.append(str(error(
                    str(PurePosixPath(directory) / "SCRUTINY"), 1,
                    "registered directory is nested under " + str(parent))))
        if registration is not None:
            for prefix in ("-experimental-", "-no-experimental-"):
                option = prefix + registration.flag
                if option in options:
                    errors.append(str(error(
                        registration.path, 1,
                        "{} collides with driver option at {}".format(
                            option, options[option]))))
    files = {directory: [] for directory in registrations}
    counts = {directory: 0 for directory in registrations}
    for path in paths:
        source = PurePosixPath(path)
        syntax = SYNTAXES.get(source.suffix)
        if syntax is None:
            continue
        owner = next((str(parent) for parent in source.parents
                      if str(parent) in registrations), None)
        if owner is not None:
            counts[owner] += 1
        try:
            stanza = read_stanza(root, path, syntax)
            if owner is None:
                if stanza is not None:
                    raise error(path, stanza.line,
                                "OxCaml header outside a registered directory")
                continue
            if stanza is None:
                raise error(path, 1, "missing OxCaml header comment")
            fields = parse_header(path, stanza)
            files[owner].append(ReviewedFile(path, fields))
            previous_fields = None
            if base is not None:
                previous_fields = {}
                previous_path = previous_paths.get(path, path)
                if previous_path is not None:
                    previous_fields = old_header(
                        root, base, previous_path, syntax)
            if models is not None:
                errors.extend(check_models(
                    path, fields, models, previous_fields))
        except CheckError as exc:
            errors.append(str(exc))
    for directory, registration in registrations.items():
        path = str(PurePosixPath(directory) / "SCRUTINY")
        if counts[directory] == 0:
            errors.append(str(error(path, 1, "registered directory has no "
                                    "tracked .ml/.mli/.v files")))
        elif (registration is not None
              and len(files[directory]) == counts[directory]):
            level = ("human-reviewed" if all(
                "human" in source.fields[FIELDS[1]].values
                for source in files[directory]) else "ai-reviewed")
            if registration.level != level:
                errors.append(str(error(
                    path, 1, "level is {}; headers imply {}; "
                    "set level {} if the headers are accurate".format(
                        registration.level, level, level))))
    if errors:
        return errors
    expected = render_status(registrations.values(), files)
    if update:
        (root / STATUS_PATH).write_text(expected, encoding="utf-8")
    else:
        try:
            actual = read_metadata(root, STATUS_PATH)
        except CheckError as exc:
            return [str(exc) + "; run python3 scrutiny/check.py --update"]
        if actual != expected:
            errors.append(str(error(
                STATUS_PATH, 1, "out of date; run "
                "python3 scrutiny/check.py --update")))
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--base", metavar="REV",
        help="PR base commit (required in CI to check retired model usage)")
    parser.add_argument("--update", action="store_true",
                        help="regenerate STATUS.md if metadata is valid")
    args = parser.parse_args()
    if args.base is None:
        print("warning: no --base supplied; retired-model changes "
              "will not be checked", file=sys.stderr)
    try:
        root = Path(git(Path.cwd(), "rev-parse", "--show-toplevel")
                    .decode("utf-8", "surrogateescape").rstrip("\n"))
        errors = check_repository(root, base=args.base, update=args.update)
    except (CheckError, OSError, UnicodeError) as exc:
        errors = [str(exc)]
    for message in errors:
        print(message, file=sys.stderr)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
