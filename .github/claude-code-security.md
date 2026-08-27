# Claude Code on this repo: shared security notes

Shared background for the two Claude Code workflows:

- `.github/workflows/claude.yml` -- interactive `@claude` on PRs.
- `.github/workflows/claude-review.yml` -- on-demand review via the
  `claude-review` label.

**What lives where.** This file explains *how the setup works*: the
authentication mechanism, how the `anthropics/claude-code-action` GitHub Action
behaves, and why certain capabilities are absent. The workflow files carry the
*per-line decisions* -- each grant, pin, and allowlist entry is justified
inline, next to the line it governs, and defers here only for the shared
mechanism. If you are changing a specific knob, its justification is in the
YAML; if you are asking "how does the whole thing fit together", it is here.

Keep it that way: do not move per-knob justifications into this file (they rot
when separated from the line they explain), and do not let this file grow into
"the real config, elsewhere".

Both workflows are **propose-only**: Claude never pushes. Changes come back as
GitHub suggestion blocks (one-click "Commit suggestion", authored by the human
who clicks) or as a unified diff in a comment that a human applies with
`git apply`. Everything below serves that property.

## Authentication: Workload Identity Federation (WIF)

There is **no static Anthropic API key** in this repo. Each run mints a
short-lived GitHub Actions OIDC token (a JWT signed by GitHub, valid for
minutes, whose claims describe the run) and the action sends it to Anthropic's
WIF endpoint, which verifies GitHub's signature, matches the run's claims
against our federation rule, and returns a short-lived Anthropic API token
scoped to a dedicated, spend-capped workspace. The `anthropic_*` values in the
workflows (rule id, org, service account, workspace) are **identifiers, not
credentials** -- they select which federation rule applies, they do not
authorize anything -- so they are safe to commit. They live in repo Actions
*variables* as a single source of truth across the two files.

Requesting that OIDC JWT is the sole purpose of the `id-token: write`
permission. Despite the name, it grants **no** write access to anything on
GitHub: the OIDC token is an *identity assertion* about the run (a JWT whose
claims describe repository, ref, workflow, actor), not an authorization, and it
conveys zero GitHub API access. `write` here means only "this job may request
such a token about itself"; there is no read variant (the permission is
`write|none`).

Anthropic token lifetime is short (minutes); the action refreshes the OIDC
token file in the background so long runs keep working. A leaked Anthropic
token therefore expires quickly, and the minting path only works from inside
this repo's own workflow runs.

## No write-capable GitHub token exists in the job

This is what makes the workflows propose-only, and it takes **two** independent
settings -- either alone leaves a write-capable token in the job:

1. **Workflow `permissions: contents: read`** scopes the workflow's own
   `GITHUB_TOKEN`.
2. **Passing `github_token: ${{ github.token }}` to the action.** Without this,
   the action's default path exchanges the run's OIDC token with Anthropic's
   backend for a **GitHub App installation token**, and that exchange requests
   `contents: write` *unconditionally* -- independent of the workflow's
   `permissions:` block. Passing our own token short-circuits that exchange
   (the action returns the provided token before ever calling it).

So `contents: read` alone is a trap: the obvious knob does not govern the
non-obvious credential. Both lines together give the invariant: no
write-capable GitHub token exists anywhere in the job. The only tradeoff is
cosmetic -- comments post as `github-actions[bot]` rather than `claude[bot]`.

## The Claude GitHub App is deliberately NOT installed

The app exists only to perform the token exchange we bypass; installing it
would be a standing `contents: write` grant on this repo exercisable by
Anthropic's backend. Its absence is also a tripwire: if someone later removes
the `github_token` line, the action fails loudly at the app exchange ("app not
installed") rather than silently acquiring a write token.

## Who can trigger Claude

Only human accounts with **write access** to this repo can trigger either
workflow, enforced by the action itself (not merely by our trigger conditions):
`checkWritePermissions` runs in the action's `prepare` step for every entity
context (both tag and agent mode), and `checkHumanActor` rejects bot actors
absent from the `allowed_bots` list. We pin the weakening knobs --
`allowed_non_write_users: ""` and `allowed_bots: ""` -- empty in both files so
loosening them requires a visible diff.

`allowed_bots` warrants caution on a public repo: allowed bots are **not**
checked for repo permissions, so `'*'` would mean roughly "anyone willing to
register a GitHub App can trigger the action". Keep it empty.

## Fork PRs are not run

Neither workflow runs Claude against code from a forked repository. The two
enforce this differently because their triggers differ:

- **`claude-review.yml`** (`pull_request: labeled`): the payload carries
  `pull_request.head.repo.full_name`, so a one-line job `if:` excludes forks.
  Independently, GitHub downgrades a fork PR run's write permissions to read,
  and since `id-token` has no read level, WIF cannot authenticate on a fork PR
  at all -- a second, independent layer.

- **`claude.yml`** (comment/review events): these are **base-repository**
  events that run with full permissions even when the comment is on a fork PR
  -- GitHub's fork downgrade does **not** apply to them. So this workflow
  rejects fork PRs with an explicit guard step that asks the API for the PR's
  head repo and fails the job if it is not this repo. (An API call is needed
  because the comment-event payloads do not carry the head repo, unlike the
  `pull_request` payload.)

Background on why running workflows against fork-controlled code in a privileged
context is dangerous ("pwn requests"):
https://securitylab.github.com/resources/github-actions-preventing-pwn-requests/

## Tag mode vs. agent mode (why the two files differ)

The action auto-detects its mode from whether a `prompt` input is supplied, and
the mode drives most of the differences between the two workflows:

- **Tag mode** (`claude.yml`, no `prompt`): responds to `@claude` mentions.
  Creates an auto-tracking progress comment, pre-fetches PR data into the
  prompt, and prepends a baseline toolset (read-only file tools, its
  comment-update and CI MCP tools, and git commit/push grants that are *inert*
  against our read-only token) with `--permission-mode acceptEdits`, which
  permits file edits inside the checkout only (wanted, so Claude can build and
  test before proposing a diff).

- **Agent mode** (`claude-review.yml`, has `prompt`): runs the prompt directly.
  Creates no tracking comment and does not pre-fetch the diff, which is why the
  review workflow needs `gh pr diff`/`gh pr view` to read the PR and
  `gh pr comment` to post its summary -- capabilities tag mode provides for
  free. Those are narrowly scoped subcommands riding the
  `pull-requests: write` / `contents: read` grants already held; deliberately
  not a general `Bash(gh:*)` (which would expose `gh api` to any endpoint).

In both modes the CI-reading tools (`mcp__github_ci__*`) mount only when they
appear in `--allowedTools` *and* the workflow token passes the action's runtime
`actions: read` probe.

## `.claude/settings.json` cannot widen permissions in CI

The repo's `.claude/settings.json` is read from the checked-out ref, which on a
PR is PR-head content. It cannot *widen* Claude's permissions in CI: `allow`
rules in a committed settings file take effect only after the interactive
workspace-trust dialog is accepted, and in the non-interactive mode the action
uses, no dialog appears and those rules stay ignored. Its `deny`/`ask` rules do
still apply (they only restrict). The authoritative tool policy therefore lives
in the workflow files (on the default branch), not in settings.json.

Receipts: Claude Code permissions docs (workspace-trust + non-interactive
paragraphs); `settingSources: [user, project, local]` in
`base-action/src/parse-sdk-options.ts`.

## Untrusted input

PR titles, bodies, comments, and review text are attacker-influenced on a
public repo and are fed to Claude. The action does best-effort sanitization of
hidden-markdown tricks (HTML comments, invisible characters), but that is not
the boundary. The boundary is the rest of this document: with no write token
and no fork runs, and with triggering limited to write-access humans, a
successful injection can post comments and consume the capped Anthropic budget
-- it cannot push code.

## Explicitly prohibited actions

None of the grants these require are present in either workflow; listed with the
grant each would need, as a checklist against accidental future widening:

- **Pushing commits / editing files / creating branches or tags**: needs
  `contents: write`.
- **Merging a PR**: needs `contents: write` on top of `pull-requests: write`
  (every merge method creates commits and moves the base ref; there is no
  fast-forward PR merge).
- **Creating or approving PRs**: gated by the org/repo setting "Allow GitHub
  Actions to create and approve pull requests", which is default-off for
  organizations and must stay off.
- **Creating issues**: needs `issues: write` (these workflows do not use
  issues).
- **Modifying workflow files**: `workflows` is not a grantable scope on the
  `permissions:` key, and is moot since contents is read-only.
- **Poisoning the Actions cache**: these workflows write no Actions cache. Nix
  dependencies are substituted from the `oxcaml` Cachix cache over HTTPS,
  read-only (no auth token in these jobs). There is no `permissions:`-level way
  to drop cache-write unconditionally (cache access uses the runner's
  `ACTIONS_RUNTIME_TOKEN`, outside the permissions system); we rely on "writes
  no cache" plus GitHub's read-only-cache-token behavior for
  untrusted/default-branch triggers.

## The Cachix cache

The `oxcaml` Cachix cache is a CI build-time optimization only. These workflows
read from it (no auth token); write access is gated to trusted refs
(push/merge_group) in `nix-github-actions.yml`, so PR builds cannot populate it.
