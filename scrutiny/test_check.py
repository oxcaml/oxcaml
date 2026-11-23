"""Run with: python3 -m unittest discover -s scrutiny -v."""

import io
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

import check


MODELS = "model\nmodel:high\nold retired\nold:high retired\n"
HEADER = ("(*= OxCaml-Written-By: human\n"
          "    OxCaml-Review-Status: ai:model:high *)\n")
REGISTRATION = "flag sample\nlevel ai-reviewed\n"


def first_stanza(text, syntax=check.OCAML):
    return check.first_stanza(io.StringIO(text), syntax)


def header(text, syntax=check.OCAML):
    return check.parse_header("sample.ml", first_stanza(text, syntax))


def retirement_errors(old, current, models=MODELS):
    return check.check_models(
        "a.ml", header(current), check.parse_models(models), header(old))


class MetadataTests(unittest.TestCase):
    def test_models_comments_and_separate_writing_and_review_permissions(self):
        self.assertEqual(check.parse_models(
            "# allowlist\nmodel # writing\nmodel:high retired\n\n"),
            {"model": False, "model:high": True})
        self.assertEqual(check.parse_models(""), {})

    def test_models_reject_malformed_and_duplicate_entries(self):
        for text in ("Model", "model:", ":high", "model:a:b", "model retired x",
                     "model inactive", "model retired retired", "model\nmodel",
                     "model\nmodel retired", "model:high\nmodel:high retired"):
            with self.subTest(text=text):
                with self.assertRaises(check.CheckError):
                    check.parse_models(text)

    def test_registration_comments_and_key_order(self):
        self.assertEqual(check.parse_registration(
            "a/SCRUTINY", "level human-reviewed # level\n\nflag a.b-1\n"),
            check.Registration("a/SCRUTINY", "a.b-1", "human-reviewed"))

    def test_registration_rejects_invalid_metadata(self):
        cases = (
            ("", "missing flag"),
            ("flag a", "missing level"),
            ("level ai-reviewed", "missing flag"),
            (REGISTRATION + "flag b", "duplicate key"),
            (REGISTRATION + "level ai-reviewed", "duplicate key"),
            (REGISTRATION + "unknown x", "unknown key"),
            ("flag a extra\nlevel ai-reviewed", "expected flag"),
            ("flag A\nlevel ai-reviewed", "expected flag"),
            ("flag a\nlevel none", "unknown review level"),
        )
        for text, message in cases:
            with self.subTest(text=text):
                with self.assertRaisesRegex(check.CheckError, message):
                    check.parse_registration("a/SCRUTINY", text)
        for name in check.RESERVED_FLAGS:
            with self.subTest(name=name):
                with self.assertRaisesRegex(check.CheckError, "reserved flag"):
                    check.parse_registration(
                        "a/SCRUTINY", "flag " + name + "\nlevel ai-reviewed")

    def test_header_sets_and_locations(self):
        fields = header("(* License *)\n" + HEADER)
        self.assertEqual(fields[check.FIELDS[0]],
                         check.HeaderField(2, ("human",)))
        self.assertEqual(fields[check.FIELDS[1]].values, ("ai:model:high",))
        self.assertEqual(fields[check.FIELDS[1]].line, 3)

    def test_header_rejects_invalid_fields(self):
        cases = (
            (HEADER.replace("human", ""), "nonempty set"),
            (HEADER.replace("human", "human,"), "nonempty set"),
            (HEADER.replace("human", "human,,ai:model"), "nonempty set"),
            (HEADER.replace("human", "human, human"), "duplicate value"),
            (HEADER.replace("human", "ai:model:high"), "invalid"),
            (HEADER.replace("ai:model:high", "ai:model"), "invalid"),
            (HEADER.replace("ai:model:high", "human"), "at least one ai:"),
            (HEADER.replace("Written-By", "Unknown"), "unknown header field"),
            (HEADER.replace("Written-By:", "Written-By"), "malformed"),
            ("(*= OxCaml-Written-By: human *)",
             "missing OxCaml-Review-Status"),
            (HEADER.replace("*)", "\nOxCaml-Written-By: human *)"),
             "duplicate"),
            (HEADER.replace("*)", ""), "unterminated"),
            (HEADER.replace("(*=", "(*"), "must open with"),
        )
        for text, message in cases:
            with self.subTest(text=text):
                with self.assertRaisesRegex(check.CheckError, message):
                    header(text)

    def test_first_stanza_is_not_combined_with_a_later_one(self):
        text = ("(*= OxCaml-Written-By: human *)\n"
                "(*= OxCaml-Review-Status: ai:model:high *)\n")
        with self.assertRaisesRegex(check.CheckError, "missing OxCaml-Review"):
            header(text)

    def test_stops_reading_after_the_first_complete_stanza(self):
        def lines():
            yield "(* Copyright *)\n"
            yield from HEADER.splitlines(True)
            raise AssertionError("read past the first stanza")

        stanza = check.first_stanza(lines(), check.OCAML)
        self.assertEqual(stanza.line, 2)
        self.assertEqual(stanza.text, HEADER.rstrip("\n"))
        self.assertEqual(header(HEADER + "(* OxCaml-Unknown: bad *)"),
                         header(HEADER))

    def test_skips_strings_characters_and_nested_non_header_comments(self):
        prefix = (
            'let quote = \'"\'\n'
            'let s = "escaped \\\" (* OxCaml-Unknown: fake *)"\n'
            'let raw = {tag|(* OxCaml-Unknown: fake *)|tag}\n'
            '(* outside (* nested *) "*)" {tag|*)|tag} *)\n'
        )
        self.assertIsNone(first_stanza(prefix))
        self.assertEqual(header(prefix + HEADER)[check.FIELDS[0]].line, 5)

    def test_quoted_extensions_do_not_hide_or_manufacture_headers(self):
        for opening, closing in (("{%sql|", "|}"),
                                 ("{%%sql id|", "|id}"),
                                 ("{%Foo.sql2' id|", "|id}"),
                                 ("{délim|", "|délim}")):
            with self.subTest(opening=opening):
                prefix = 'let x = ' + opening + '"' + closing + '\n'
                self.assertEqual(
                    header(prefix + HEADER)[check.FIELDS[0]].line, 2)
                fake = 'let x = ' + opening + HEADER + closing + '\n'
                self.assertIsNone(first_stanza(fake))

    def test_rocq_strings_double_quotes_and_ignore_backslashes(self):
        prefix = ('Definition a := "\\".\n'
                  'Definition b := "x "" (*= OxCaml-Unknown: fake *)".\n'
                  '(* outside (* nested *) "*)" *)\n')
        self.assertIsNone(first_stanza(prefix, check.ROCQ))
        self.assertEqual(
            header(prefix + HEADER, check.ROCQ)[check.FIELDS[0]].line, 4)
        # OCaml rules read the backslash as an escape and expose the fake.
        self.assertEqual(first_stanza(prefix, check.OCAML).line, 2)

    def test_rocq_braces_do_not_open_quoted_strings(self):
        prefix = ('Definition c := {x|x > 0}.\n'
                  'Definition d := {| e := 1 |}.\n')
        self.assertEqual(
            header(prefix + HEADER, check.ROCQ)[check.FIELDS[0]].line, 3)
        # OCaml rules read {x| as opening a quoted string that never closes.
        self.assertIsNone(first_stanza(prefix + HEADER, check.OCAML))

    def test_ordinary_oxcaml_prose_is_not_a_header_stanza(self):
        prose = ("(* A regression test that is OxCaml-only. *)\n"
                 "(** OxCaml-compatible version of perform. *)\n"
                 "(* Exercise of OxCaml-specific constructs. *)\n")
        self.assertIsNone(first_stanza(prose))
        self.assertEqual(header(prose + HEADER)[check.FIELDS[0]].line, 4)
        self.assertEqual(header(HEADER.replace(
            "human\n", "human\n   (* OxCaml-specific code *)\n"))
            [check.FIELDS[1]].line, 3)

    def test_header_comment_can_contain_nested_comments(self):
        text = HEADER.replace("human\n", "human\n   (* explanation *)\n")
        self.assertEqual(header(text)[check.FIELDS[1]].line, 3)

    def test_model_permissions_are_independent(self):
        fields = header(HEADER.replace("human", "ai:model"))
        models = check.parse_models("model:high\n")
        self.assertEqual(check.check_models("a.ml", fields, models, None), [
            "a.ml:1: unapproved model entry model"])

    def test_retired_entries_can_stay_in_a_field_that_changes(self):
        written = HEADER.replace("human", "ai:old")
        self.assertEqual(retirement_errors(
            written, written.replace("ai:old", "ai:old, human")), [])

        def reviewed(status):
            return HEADER.replace("ai:model:high", status)

        for old, current in (
                ("ai:old:high", "ai:old:high, human"),
                ("ai:old:high, human", "ai:old:high"),
                ("ai:old:high", "ai:old:high, ai:model:high"),
                ("ai:old:high, human", "human,ai:old:high")):
            with self.subTest(old=old, current=current):
                self.assertEqual(retirement_errors(
                    reviewed(old), reviewed(current)), [])

    def test_retired_entries_survive_reformatting(self):
        old = HEADER.replace("human", "ai:old")
        for current in (old.replace("(*= ", "(*=\n    ", 1),
                        old.replace("Written-By: ", "Written-By:  "),
                        "(*= OxCaml-Review-Status: ai:model:high\n"
                        "    OxCaml-Written-By: ai:old *)\n"):
            with self.subTest(current=current):
                self.assertEqual(retirement_errors(old, current), [])

    def test_added_retired_entries_are_rejected(self):
        # The retired author is in the old header; the retired review is not.
        old = HEADER.replace("human", "ai:old")
        current = old.replace("ai:model:high", "ai:model:high, ai:old:high")
        self.assertEqual(retirement_errors(old, current), [
            "a.ml:2: header adds retired entry old:high"])
        self.assertEqual(
            retirement_errors(old, old.replace("ai:old", "ai:older"),
                              models=MODELS + "older retired\n"),
            ["a.ml:1: header adds retired entry older"])

    def test_retirement_needs_a_base_with_a_header(self):
        fields = header(HEADER.replace("human", "ai:old"))
        models = check.parse_models(MODELS)
        self.assertEqual(check.check_models("a.ml", fields, models, None), [])
        self.assertEqual(check.check_models("a.ml", fields, models, {}), [
            "a.ml:1: header adds retired entry old"])


class RepositoryTests(unittest.TestCase):
    # Index changes and commits stay in these temporary repositories.
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="scrutiny-test-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.git("init", "-q")
        self.write("scrutiny/models", MODELS)
        self.write("feature/SCRUTINY", REGISTRATION)
        self.write("feature/a.ml", HEADER + "let x = 1\n")
        self.git("add", ".")
        self.assertEqual(check.check_repository(self.root, update=True), [])
        self.commit()
        self.base = self.git("rev-parse", "HEAD").decode().strip()

    def git(self, *args):
        return check.git(self.root, *args)

    def write(self, path, text):
        target = self.root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding="utf-8")

    def commit(self):
        self.git("add", ".")
        self.git("-c", "user.name=Scrutiny tests",
                 "-c", "user.email=scrutiny-tests@example.invalid",
                 "-c", "commit.gpgSign=false", "-c", "core.hooksPath=/dev/null",
                 "commit", "-qm", "Test snapshot")

    def run_check(self, **kwargs):
        return check.check_repository(self.root, base=self.base, **kwargs)

    def assert_error(self, fragment, **kwargs):
        errors = self.run_check(**kwargs)
        self.assertTrue(any(fragment in message for message in errors), errors)

    def test_check_update_and_deterministic_table(self):
        expected = (
            "# Experimental feature review status\n\n"
            "Generated by `scrutiny/check.py`; do not edit by hand.\n\n"
            "| Feature | Directory | Level | File | "
            "Written by | Reviewed by |\n"
            "|---|---|---|---|---|---|\n"
            "| sample | feature | ai-reviewed | feature/a.ml | human | "
            "ai:model:high |\n"
        )
        self.assertEqual((self.root / check.STATUS_PATH).read_text(), expected)
        self.assertEqual(self.run_check(), [])
        self.write(check.STATUS_PATH, "stale\n")
        self.assert_error("out of date")
        self.assertEqual(self.run_check(update=True), [])
        self.assertEqual((self.root / check.STATUS_PATH).read_text(), expected)
        self.assertEqual(self.run_check(), [])

    def test_update_never_repairs_invalid_metadata_or_overwrites_status(self):
        bad_registration = REGISTRATION.replace("ai-reviewed", "human-reviewed")
        self.write("feature/SCRUTINY", bad_registration)
        self.write(check.STATUS_PATH, "keep me\n")
        self.assert_error("headers imply ai-reviewed", update=True)
        self.assertEqual((self.root / check.STATUS_PATH).read_text(),
                         "keep me\n")
        self.assertEqual((self.root / "feature/SCRUTINY").read_text(),
                         bad_registration)
        self.assertEqual((self.root / "feature/a.ml").read_text(),
                         HEADER + "let x = 1\n")

    def test_reports_errors_in_multiple_files(self):
        self.write("feature/a.ml", "let x = 1\n")
        self.write("feature/b.mli", "val x : int\n")
        self.git("add", "feature/b.mli")
        errors = self.run_check()
        self.assertEqual(errors, [
            "feature/a.ml:1: missing OxCaml header comment",
            "feature/b.mli:1: missing OxCaml header comment",
        ])

    def test_invalid_model_allowlist_is_checked(self):
        self.write("scrutiny/models", MODELS + "model retired\n")
        self.assert_error("duplicate entry model")

    def test_tracked_files_only_and_descendants_are_included(self):
        self.write("feature/untracked.ml", "no header")
        self.write("untracked/SCRUTINY", "invalid")
        self.write("untracked/a.ml", HEADER)
        self.assertEqual(self.run_check(), [])
        self.write("feature/sub/a.mli", HEADER + "val x : int\n")
        self.write("feature/sub/a.v", HEADER + "Definition x := 1.\n")
        self.write("feature/sub/script.py", "no header needed\n")
        self.git("add", "feature/sub")
        self.assertEqual(self.run_check(update=True), [])
        status = (self.root / check.STATUS_PATH).read_text()
        self.assertIn("feature/sub/a.mli", status)
        self.assertIn("feature/sub/a.v", status)
        self.assertNotIn("untracked", status)
        self.assertNotIn("script.py", status)

    def test_header_outside_registered_directory_is_rejected(self):
        for name, text in (("outside.ml", HEADER),
                           ("plain.ml", HEADER.replace("(*=", "(*")),
                           ("single.ml", "(*= OxCaml-Written-By: human *)\n")):
            with self.subTest(name=name):
                self.write(name, text)
                self.git("add", name)
                self.assert_error(name + ":1: OxCaml header outside")

    def test_header_text_inside_a_string_is_not_a_header(self):
        self.write("outside.ml", 'let s = {|\n' + HEADER + '|}\n')
        self.git("add", "outside.ml")
        self.assertEqual(self.run_check(), [])

    def test_quoted_extension_does_not_hide_an_unregistered_header(self):
        self.write("outside.ml", 'let x = {%sql|"|}\n' + HEADER)
        self.git("add", "outside.ml")
        self.assert_error("outside.ml:2: OxCaml header outside")

    def test_lexical_rules_follow_the_file_extension(self):
        text = 'Definition s := "\\".\n' + HEADER
        for name in ("outside.ml", "outside.v"):
            self.write(name, text)
            self.git("add", name)
        self.assertEqual(self.run_check(), [
            "outside.v:2: OxCaml header outside a registered directory"])

    def test_non_utf8_source_without_headers_is_supported(self):
        (self.root / "outside.ml").write_bytes(b'let s = "\xff"\n')
        self.git("add", "outside.ml")
        self.assertEqual(self.run_check(), [])

    def test_nested_registrations_are_rejected(self):
        self.write("feature/sub/SCRUTINY",
                   REGISTRATION.replace("sample", "sub"))
        self.write("feature/sub/a.ml", HEADER)
        self.git("add", "feature/sub")
        self.assert_error("registered directory is nested under feature")

    def test_empty_directory_and_duplicate_flag_are_rejected(self):
        self.write("other/SCRUTINY", REGISTRATION)
        self.git("add", "other")
        errors = self.run_check()
        self.assertTrue(any("duplicate flag sample" in e for e in errors),
                        errors)
        self.assertTrue(
            any("no tracked .ml/.mli/.v files" in e for e in errors), errors)

    def test_root_registration_owns_files_and_disallows_nesting(self):
        self.write("SCRUTINY", REGISTRATION.replace("sample", "root"))
        self.git("add", "SCRUTINY")
        self.assert_error("registered directory is nested under .")
        self.git("rm", "feature/SCRUTINY")
        self.assertEqual(self.run_check(update=True), [])

    def test_each_generated_option_is_checked_for_collisions(self):
        for prefix in ("-experimental-", "-no-experimental-"):
            with self.subTest(prefix=prefix):
                self.write("driver/args.ml", 'let opt = ("' + prefix
                           + 'sample", Arg.Unit f, "")\n')
                self.git("add", "driver/args.ml")
                self.assert_error(prefix + "sample collides with driver option")

    def test_ordinary_options_and_comment_or_help_mentions_do_not_collide(self):
        self.write("driver/args.ml", (
            '(* "-experimental-sample" *)\n'
            'let opt = ("-sample", Arg.Unit f, "Use -experimental-sample")\n'
        ))
        self.git("add", "driver/args.ml")
        self.assertEqual(self.run_check(), [])

    def test_weakest_file_determines_level(self):
        self.write("feature/a.ml", HEADER.replace(
            "ai:model:high", "human, ai:model:high"))
        self.assert_error("headers imply human-reviewed")
        self.write("feature/sub/b.mli", HEADER)
        self.git("add", "feature/sub/b.mli")
        self.assertEqual(self.run_check(update=True), [])
        self.write("feature/sub/b.mli", HEADER.replace(
            "ai:model:high", "ai:model:high, human"))
        self.write("feature/SCRUTINY", REGISTRATION.replace(
            "ai-reviewed", "human-reviewed"))
        self.assertEqual(self.run_check(update=True), [])

    def test_retired_authorship_survives_body_and_active_review_changes(self):
        old = HEADER.replace("human", "ai:old")
        self.write("feature/a.ml", old + "let x = 1\n")
        self.commit()
        self.base = self.git("rev-parse", "HEAD").decode().strip()
        self.write("feature/a.ml", old + "let x = 2\n")
        self.assertEqual(self.run_check(update=True), [])
        self.write("feature/a.ml", old.replace(
            "ai:model:high", "ai:model:high, human") + "let x = 2\n")
        self.write("feature/SCRUTINY", REGISTRATION.replace(
            "ai-reviewed", "human-reviewed"))
        self.assertEqual(self.run_check(update=True), [])

    def test_adding_retired_entries_to_old_or_new_files_is_rejected(self):
        old = HEADER.replace("human", "ai:old")
        self.write("feature/a.ml", old)
        self.assert_error("retired entry old")
        self.write("feature/a.ml", HEADER)
        self.write("feature/new.ml", old)
        self.git("add", "feature/new.ml")
        self.assert_error("feature/new.ml:1: header adds retired entry old")

    def test_human_review_and_rewrite_can_join_retired_entries(self):
        old = HEADER.replace("human", "ai:old").replace(
            "ai:model:high", "ai:old:high")
        self.write("feature/a.ml", old)
        self.commit()
        self.base = self.git("rev-parse", "HEAD").decode().strip()
        self.write("feature/a.ml", old.replace(
            "ai:old\n", "ai:old, human\n").replace(
            "ai:old:high", "ai:old:high, human"))
        self.write("feature/SCRUTINY", REGISTRATION.replace(
            "ai-reviewed", "human-reviewed"))
        self.assertEqual(self.run_check(update=True), [])

    def test_invalid_base_header_vouches_for_no_retired_entries(self):
        old = HEADER.replace("human", "ai:old")
        self.write("feature/a.ml", old.replace("(*=", "(*"))
        self.commit()
        self.base = self.git("rev-parse", "HEAD").decode().strip()
        self.write("feature/a.ml", old)
        self.assert_error("feature/a.ml:1: header adds retired entry old")
        self.write("feature/a.ml", HEADER)
        self.assertEqual(self.run_check(), [])

    def test_renaming_preserves_historical_header_lines(self):
        self.write("feature/a.ml", HEADER.replace("human", "ai:old"))
        self.commit()
        self.base = self.git("rev-parse", "HEAD").decode().strip()
        self.git("mv", "feature/a.ml", "feature/renamed.ml")
        self.assertEqual(self.run_check(update=True), [])

    def test_deleted_files_do_not_contribute_to_level_or_status(self):
        self.write("feature/b.ml", HEADER.replace(
            "ai:model:high", "ai:model:high, human"))
        self.git("add", "feature/b.ml")
        self.git("rm", "feature/a.ml")
        self.write("feature/SCRUTINY", REGISTRATION.replace(
            "ai-reviewed", "human-reviewed"))
        self.assertEqual(self.run_check(update=True), [])
        self.assertNotIn("feature/a.ml",
                         (self.root / check.STATUS_PATH).read_text())

    def test_status_handles_paths_with_spaces_and_table_delimiters(self):
        self.git("mv", "feature/a.ml", "feature/a | b.ml")
        self.assertEqual(self.run_check(update=True), [])
        self.assertIn("feature/a &#124; b.ml",
                      (self.root / check.STATUS_PATH).read_text())

    def test_no_registered_features_and_empty_allowlist(self):
        self.git("rm", "-r", "feature")
        self.write("scrutiny/models", "")
        self.assertEqual(self.run_check(update=True), [])
        self.assertIn("No experimental features are registered.",
                      (self.root / check.STATUS_PATH).read_text())

    def test_missing_status_suggests_regeneration(self):
        (self.root / check.STATUS_PATH).unlink()
        self.assert_error("run python3 scrutiny/check.py --update")
        self.assertEqual(self.run_check(update=True), [])

    def test_invalid_base_fails_instead_of_skipping_retirement_checks(self):
        with self.assertRaisesRegex(check.CheckError, "git rev-parse"):
            check.check_repository(self.root, base="nonexistent-base")

    def test_invalid_base_cannot_be_interpreted_as_a_git_option(self):
        for base in ("", "--help"):
            with self.subTest(base=base):
                with self.assertRaisesRegex(check.CheckError, "--base must"):
                    check.check_repository(self.root, base=base)

    def test_cli_from_subdirectory_and_exit_statuses(self):
        script = str(Path(check.__file__).resolve())
        command = [sys.executable, "-B", script, "--base", self.base]
        result = subprocess.run(command, cwd=str(self.root / "feature"),
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.assertEqual((result.returncode, result.stderr), (0, b""))
        self.write("feature/a.ml", "no header")
        result = subprocess.run(command, cwd=str(self.root),
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.assertEqual(result.returncode, 1)
        self.assertIn(b"missing OxCaml header", result.stderr)
        self.assertNotIn(b"Traceback", result.stderr)

    def test_cli_warns_when_retirement_check_is_skipped(self):
        result = subprocess.run(
            [sys.executable, "-B", str(Path(check.__file__).resolve())],
            cwd=str(self.root), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.assertEqual(result.returncode, 0)
        self.assertIn(b"no --base supplied", result.stderr)


if __name__ == "__main__":
    unittest.main()
