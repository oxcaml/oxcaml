#!/usr/bin/env python3
"""Find or create a PENDING review on a PR.

Usage:
    find-or-create-pending-review.py --pr N [--repo OWNER/REPO]

Arguments:
    --pr     PR number (required, positive integer).
    --repo   GitHub repo in OWNER/REPO form. Defaults to oxcaml/oxcaml.

Output:
    A single JSON object on stdout with two fields:
      node_id   GraphQL node id of the pending review
                (pass to post-pending-comment.py --review-id).
      rest_id   REST numeric id of the pending review
                (pass to list-review-comments.py --review-id).

If a PENDING review already exists on the PR, its ids are returned. Otherwise
a new review is created with no `event` field, which leaves it in PENDING
state. The script never submits a review.
"""

import argparse
import json
import re
import subprocess
import sys

REPO_RE = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")


def gh(*args):
    result = subprocess.run(
        ["gh", *args],
        capture_output=True,
        check=False,
    )
    if result.returncode != 0:
        sys.stderr.write(result.stderr.decode("utf-8", errors="replace"))
        sys.exit(result.returncode)
    return result.stdout


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pr", type=int, required=True)
    parser.add_argument("--repo", default="oxcaml/oxcaml")
    args = parser.parse_args()

    if not REPO_RE.match(args.repo):
        sys.stderr.write(f"Refusing suspicious --repo value: {args.repo!r}\n")
        sys.exit(2)
    if args.pr <= 0:
        sys.stderr.write(f"Refusing non-positive --pr value: {args.pr}\n")
        sys.exit(2)

    endpoint = f"repos/{args.repo}/pulls/{args.pr}/reviews"

    reviews = json.loads(gh("api", endpoint))
    for r in reviews:
        if r.get("state") == "PENDING":
            print(json.dumps({"node_id": r["node_id"], "rest_id": r["id"]}))
            return

    created = json.loads(
        gh("api", endpoint, "--method", "POST", "-f", "body=")
    )
    print(json.dumps({"node_id": created["node_id"], "rest_id": created["id"]}))


if __name__ == "__main__":
    main()
