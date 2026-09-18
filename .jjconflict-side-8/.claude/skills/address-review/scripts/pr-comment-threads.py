#!/usr/bin/env python3
"""Fetch review comment threads for a GitHub PR."""

import argparse
import json
import subprocess
import sys

ALLOWED_REPOS = {"oxcaml/oxcaml", "ocaml/ocaml"}

QUERY = """
query($owner: String!, $repo: String!, $pr: Int!, $cursor: String) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $pr) {
      reviewThreads(first: 100, after: $cursor) {
        pageInfo {
          hasNextPage
          endCursor
        }
        nodes {
          isResolved
          isOutdated
          path
          line
          originalLine
          startLine
          originalStartLine
          diffSide
          comments(first: 100) {
            nodes {
              body
              author { login }
              createdAt
              diffHunk
            }
          }
        }
      }
    }
  }
}
"""


def main():
    parser = argparse.ArgumentParser(
        description="Fetch unresolved review threads for a GitHub PR"
    )
    parser.add_argument("--pr", type=int, required=True, help="PR number")
    parser.add_argument(
        "--repo",
        required=True,
        help="Repository in owner/repo format",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Include resolved threads (default: only unresolved)",
    )
    args = parser.parse_args()

    if args.pr <= 0:
        print("Error: PR number must be positive", file=sys.stderr)
        sys.exit(1)

    if args.repo not in ALLOWED_REPOS:
        print(f"Error: repo must be one of: {', '.join(sorted(ALLOWED_REPOS))}", file=sys.stderr)
        sys.exit(1)

    owner, repo = args.repo.split("/")

    all_threads = []
    cursor = None

    while True:
        cmd = [
            "gh", "api", "graphql",
            "-f", "query=" + QUERY,
            "-f", "owner=" + owner,
            "-f", "repo=" + repo,
            "-F", "pr=" + str(args.pr),
        ]
        if cursor:
            cmd.extend(["-f", "cursor=" + cursor])

        result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

        if result.returncode != 0:
            print(result.stderr.decode(), file=sys.stderr)
            sys.exit(result.returncode)

        data = json.loads(result.stdout.decode())
        threads_data = data["data"]["repository"]["pullRequest"]["reviewThreads"]

        for thread in threads_data["nodes"]:
            if args.all or not thread["isResolved"]:
                comments = thread["comments"]["nodes"]
                diff_hunk = comments[0]["diffHunk"] if comments else None
                all_threads.append({
                    "path": thread["path"],
                    "line": thread["line"],
                    "originalLine": thread["originalLine"],
                    "startLine": thread["startLine"],
                    "originalStartLine": thread["originalStartLine"],
                    "diffSide": thread["diffSide"],
                    "isResolved": thread["isResolved"],
                    "isOutdated": thread["isOutdated"],
                    "diffHunk": diff_hunk,
                    "comments": [
                        {
                            "author": c["author"]["login"] if c["author"] else None,
                            "body": c["body"],
                            "createdAt": c["createdAt"],
                        }
                        for c in comments
                    ],
                })

        page_info = threads_data["pageInfo"]
        if page_info["hasNextPage"]:
            cursor = page_info["endCursor"]
        else:
            break

    print(json.dumps(all_threads, indent=2))


if __name__ == "__main__":
    main()
