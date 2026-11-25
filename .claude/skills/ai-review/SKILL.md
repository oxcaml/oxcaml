---
name: ai-review
description: AI review, focused on correctness, targeted at experimental features.
allowed-tools: AskUserQuestion(*)
argument-hint: [branch, or branch stack]
---

This skill is expected to be used in order to review code before it gains
"experimental" status, as described in `scrutiny/README.md`. It proceeds in
step as follows (more details in subsections below):

- step 1: determine the scope;
- step 2: determine the review entry;
- step 3: actual review;
- step 4: recording of findings;
- step 5: suggestion of headers.

Be sure to read `scrutiny/README.md` and follow the instructions there,
in addition to the below.

Below, "the user" is the person who asked for the review; they are not
necessarily the author of the code under review.


## Step 1: determine the scope

The first thing to do is to determine the diff to review. In most cases, it
will be the diff of a branch, or stack of branches.

Once the scope is determined, it is useful to have a first, very superficial
look, to see whether obvious questions should be raised to further determine
the scope or intent of the review. Such questions should ideally be batched,
rather than intertwined with review.


## Step 2: determine the review entry

A review is recorded in file headers as `ai:<model>:<effort>`, where
`<model>:<effort>` must be an active entry of `scrutiny/models`, spelled as
it is there. Read that file, suggest the entry this session corresponds to,
and always ask the user to validate it before starting the review. The
session may not know its effort, or may know it in another form than the
one used in `scrutiny/models`; the user is the one who chose the settings.
This question can be batched with those of step 1.

If the session corresponds to no active entry, the review cannot be
recorded in headers. Tell the user, and carry on only if they still want
the findings; step 5 is then skipped.


## Step 3: actual review

The focus of the review is correctness. Style issues should be mentioned iff
egregious and/or very likely to result in maintenance burden. In order to
meaningfully check correctness, it is important to follow code dependencies
to build a comprehensive understanding of the code.

If needed, it is also fine to build and run the compiler in order to confirm
or falsify hypothesis. There is no need to seek agreement, unless the job(s)
is expected to take a long time (e.g. more than 1 hour).


## Step 4: recording of findings

All findings should be recorded in a single fresh Markdown file at the root
of the repository. This file is not expected to be committed and should have
a file name following the `yyyy-mm-dd-REVIEW-xyz.md` convention (`yyyy-mm-dd`
prefix is the date, and `xyz` suffix is a short description of the theme).
It is not committed because it is the user who has the responsibility of
triaging and evaluating the findings, and eventually of asking the author
for changes.

At the top of the file, the following should be recorded:

- the entry validated in step 2 (or the absence of one), together with the
  model and effort as the session reports them;
- the base and head commits of the diff reviewed;
- the files covered that live in a registered directory, i.e. those whose
  headers the review bears on.

Then the rest of the file is:

- a short (5-10 lines) paragraph summarizing the findings;
- a section per finding, with the most critical issues first.

For each finding, we are interested in: its criticality, a short description,
a reproduction case if any, and possible fixes and/or workarounds. It is fine
to report "hypothetical" findings, i.e. findings with no reproduction cases,
and areas where you think another reviewer (AI or human) should have a second
look.


## Step 5: suggestion of headers

Unless step 2 found no entry, finish by editing the files so that they
record the review, as a suggestion to the user:

- add the validated entry to `OxCaml-Review-Status` in the header of each
  registered file covered by the review, leaving the other values and
  `OxCaml-Written-By` as they are;
- if needed, update the `level` line of the `SCRUTINY` file and regenerate
  `scrutiny/STATUS.md` (`python3 scrutiny/check.py --update`).

Do not stage or commit these edits. They are only a suggestion, and they
are valid only once the findings have been addressed: say so explicitly,
both in the findings file (which should list the edits) and when reporting
to the user. Keeping the headers accurate is the user's responsibility.

Some cases should be raised with the user instead of being edited: a file
with no `OxCaml-Written-By` line (the review cannot establish the origin of
a file), and a status including `human` on a file the diff changes (that
review may no longer cover the contents). If the code under review is not
what is checked out, describe the edits in the findings file instead of
making them.
