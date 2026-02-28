#!/usr/bin/env node
"use strict";

import fs from "node:fs";
import path from "node:path";
import http from "node:http";
import os from "node:os";
import { URL } from "node:url";
import { execFileSync } from "node:child_process";
import { DatabaseSync } from "node:sqlite";

const allowedExtensions = new Set(["ml", "mli", "mll", "mly", "c", "h", "cmm", "s", "asm"]);
const pathSpecs = Array.from(allowedExtensions, (ext) => `*.${ext}`);
const maxBuffer = 1024 * 1024 * 256;
const defaultFrom = "5.3.0";
const defaultTo = "5.4.0";

const options = parseArgs(process.argv.slice(2), {
  repo: process.cwd(),
  root: path.join(process.cwd(), "ai-docs"),
  host: "127.0.0.1",
  port: "8787",
});

const repoRoot = path.resolve(String(options.repo));
const webRoot = path.resolve(String(options.root));
const host = String(options.host);
const port = Number(options.port);

if (!Number.isFinite(port) || port <= 0) {
  throw new Error(`Invalid --port: ${options.port}`);
}
if (!fs.existsSync(path.join(repoRoot, ".git"))) {
  throw new Error(`Missing git repo: ${repoRoot}`);
}
if (!fs.existsSync(webRoot) || !fs.statSync(webRoot).isDirectory()) {
  throw new Error(`Missing web root directory: ${webRoot}`);
}

const treeCache = new Map();
const treeRowsCache = new Map();
const diffCache = new Map();
const lineCountCache = new Map();
const summaryInFlight = new Map();
let refsCache = null;

const summaryRetryMs = 20_000;
const summaryDbPath = path.join(webRoot, "source-summaries.sqlite");
const summaryDb = new DatabaseSync(summaryDbPath);
summaryDb.exec(`
  PRAGMA journal_mode=WAL;
  CREATE TABLE IF NOT EXISTS summaries (
    summary_key TEXT PRIMARY KEY,
    mode TEXT NOT NULL,
    path TEXT NOT NULL,
    ref_resolved TEXT,
    from_resolved TEXT,
    to_resolved TEXT,
    status TEXT NOT NULL,
    one_line TEXT,
    paragraph TEXT,
    last_error TEXT,
    started_at_ms INTEGER NOT NULL DEFAULT 0,
    updated_at_ms INTEGER NOT NULL DEFAULT 0
  );
`);

const summarySelectStmt = summaryDb.prepare(`
  SELECT
    summary_key, mode, path, ref_resolved, from_resolved, to_resolved,
    status, one_line, paragraph, last_error, started_at_ms, updated_at_ms
  FROM summaries
  WHERE summary_key = ?
`);

const summaryUpsertPendingStmt = summaryDb.prepare(`
  INSERT INTO summaries (
    summary_key, mode, path, ref_resolved, from_resolved, to_resolved,
    status, one_line, paragraph, last_error, started_at_ms, updated_at_ms
  ) VALUES (?, ?, ?, ?, ?, ?, 'pending', NULL, NULL, NULL, ?, ?)
  ON CONFLICT(summary_key) DO UPDATE SET
    mode = excluded.mode,
    path = excluded.path,
    ref_resolved = excluded.ref_resolved,
    from_resolved = excluded.from_resolved,
    to_resolved = excluded.to_resolved,
    status = 'pending',
    one_line = NULL,
    paragraph = NULL,
    last_error = NULL,
    started_at_ms = excluded.started_at_ms,
    updated_at_ms = excluded.updated_at_ms
`);

const summaryMarkReadyStmt = summaryDb.prepare(`
  UPDATE summaries
  SET status = 'ready',
      one_line = ?,
      paragraph = ?,
      last_error = NULL,
      started_at_ms = 0,
      updated_at_ms = ?
  WHERE summary_key = ?
`);

const summaryMarkFailedStmt = summaryDb.prepare(`
  UPDATE summaries
  SET status = 'pending',
      last_error = ?,
      updated_at_ms = ?
  WHERE summary_key = ?
`);

const server = http.createServer((req, res) => {
  try {
    const requestUrl = new URL(req.url || "/", `http://${req.headers.host || `${host}:${port}`}`);
    if (requestUrl.pathname === "/api/refs") {
      const payload = getRefsPayload();
      sendJson(res, 200, payload);
      return;
    }
    if (requestUrl.pathname === "/api/tree") {
      const refName = requestUrl.searchParams.get("ref") || defaultTo;
      const payload = buildTreePayload(refName);
      sendJson(res, 200, payload);
      return;
    }
    if (requestUrl.pathname === "/api/parents") {
      const refName = requestUrl.searchParams.get("ref") || "HEAD";
      const payload = getParentsPayload(refName);
      sendJson(res, 200, payload);
      return;
    }
    if (requestUrl.pathname === "/api/file") {
      const refName = requestUrl.searchParams.get("ref") || "HEAD";
      const filePath = requestUrl.searchParams.get("path") || "";
      const payload = getFilePayload(refName, filePath);
      sendJson(res, 200, payload);
      return;
    }
    if (requestUrl.pathname === "/api/file-diff") {
      const fromRef = requestUrl.searchParams.get("from") || defaultFrom;
      const toRef = requestUrl.searchParams.get("to") || defaultTo;
      const filePath = requestUrl.searchParams.get("path") || "";
      const payload = getFileDiffPayload(fromRef, toRef, filePath);
      sendJson(res, 200, payload);
      return;
    }
    if (requestUrl.pathname === "/api/summary") {
      const fromRef = String(requestUrl.searchParams.get("from") || "").trim();
      const toRef = String(requestUrl.searchParams.get("to") || "").trim();
      const refName = String(requestUrl.searchParams.get("ref") || "").trim();
      const filePath = requestUrl.searchParams.get("path") || "";
      const trigger = String(requestUrl.searchParams.get("trigger") || "1") !== "0";
      const payload = getSummaryPayload({
        refName,
        fromRef,
        toRef,
        filePath,
        trigger,
      });
      sendJson(res, 200, payload);
      return;
    }
    if (requestUrl.pathname === "/api/merge-base") {
      const refName = requestUrl.searchParams.get("ref") || "";
      const targetName = requestUrl.searchParams.get("target") || "origin/main";
      if (!String(refName).trim()) {
        sendJson(res, 400, { error: "Missing ref query parameter" });
        return;
      }
      const payload = getMergeBasePayload(refName, targetName);
      sendJson(res, 200, payload);
      return;
    }
    if (requestUrl.pathname === "/api/diff") {
      const fromRef = requestUrl.searchParams.get("from") || defaultFrom;
      const toRef = requestUrl.searchParams.get("to") || defaultTo;
      const sizeBasisRaw = requestUrl.searchParams.get("sizeBasis") || "max";
      const sizeBasis = normalizeSizeBasis(sizeBasisRaw);
      if (!sizeBasis) {
        sendJson(res, 400, { error: `Invalid sizeBasis: ${sizeBasisRaw}` });
        return;
      }
      const payload = buildDiffPayload(fromRef, toRef, sizeBasis);
      sendJson(res, 200, payload);
      return;
    }
    serveStatic(requestUrl.pathname, res);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    sendJson(res, 500, { error: message });
  }
});

server.listen(port, host, () => {
  process.stdout.write(
    `Treemap server running at http://${host}:${port}/source-treemap.html\nRepo: ${repoRoot}\nRoot: ${webRoot}\n`
  );
});

function parseArgs(argv, defaults) {
  const result = { ...defaults };
  for (const arg of argv) {
    const equal = arg.indexOf("=");
    if (!arg.startsWith("--") || equal < 0) continue;
    const key = arg.slice(2, equal).trim();
    const value = arg.slice(equal + 1).trim();
    if (!key) continue;
    result[key] = value;
  }
  return result;
}

function gitText(args) {
  return execFileSync("git", ["-C", repoRoot, ...args], {
    encoding: "utf8",
    maxBuffer,
  });
}

function gitBuffer(args, inputText) {
  return execFileSync("git", ["-C", repoRoot, ...args], {
    input: inputText,
    maxBuffer,
  });
}

function extensionOf(filePath) {
  return path.extname(filePath).replace(/^\./, "").toLowerCase();
}

function isTrackableSourcePath(filePath) {
  const rel = String(filePath || "");
  if (!rel) return false;
  if (rel.startsWith("_install/") || rel.startsWith("/_install/")) return false;
  return allowedExtensions.has(extensionOf(rel));
}

function normalizeRepoPath(filePath) {
  const raw = String(filePath || "").trim().replace(/\\/g, "/");
  if (!raw) throw new Error("Missing file path");
  if (raw.startsWith("/") || raw.includes("\0")) {
    throw new Error(`Invalid file path: ${raw}`);
  }
  const parts = raw.split("/");
  if (parts.some((part) => !part || part === "." || part === "..")) {
    throw new Error(`Invalid file path: ${raw}`);
  }
  const normalized = parts.join("/");
  if (!isTrackableSourcePath(normalized)) {
    throw new Error(`Unsupported or untracked source path: ${normalized}`);
  }
  return normalized;
}

function normalizeSizeBasis(raw) {
  if (raw === "before" || raw === "after" || raw === "max") return raw;
  return "";
}

function chooseLayoutBytes(bytesBefore, bytesAfter, sizeBasis) {
  if (sizeBasis === "before") return bytesBefore;
  if (sizeBasis === "after") return bytesAfter;
  return Math.max(bytesBefore, bytesAfter);
}

function resolveCommit(refName) {
  const ref = String(refName || "").trim();
  if (!ref) throw new Error("Empty commit ref");
  const resolved = gitText(["rev-parse", "--verify", `${ref}^{commit}`]).trim();
  if (!resolved) throw new Error(`Cannot resolve ref: ${ref}`);
  return resolved;
}

function parseLsTreeRows(output) {
  const map = new Map();
  const lines = output.split("\n");
  for (const line of lines) {
    if (!line) continue;
    const tab = line.indexOf("\t");
    if (tab < 0) continue;
    const filePath = line.slice(tab + 1);
    if (!isTrackableSourcePath(filePath)) continue;
    const meta = line.slice(0, tab).trim().split(/\s+/);
    if (meta.length < 4) continue;
    const oid = meta[2];
    const sizeRaw = meta[3];
    if (sizeRaw === "-") continue;
    const bytes = Number(sizeRaw);
    if (!Number.isFinite(bytes)) continue;
    map.set(filePath, {
      bytes,
      oid,
      extension: extensionOf(filePath),
    });
  }
  return map;
}

function getTreeForCommit(commitSha) {
  const hit = treeCache.get(commitSha);
  if (hit) return hit;
  const tree = parseLsTreeRows(gitText(["ls-tree", "-r", "-l", commitSha]));
  treeCache.set(commitSha, tree);
  return tree;
}

function getTreeRowsForCommit(commitSha) {
  const cached = treeRowsCache.get(commitSha);
  if (cached) return cached;
  const tree = getTreeForCommit(commitSha);
  const rows = [];
  for (const [filePath, info] of tree) {
    rows.push({
      path: filePath,
      bytes: info.bytes,
      bytesBefore: info.bytes,
      bytesAfter: info.bytes,
      extension: info.extension,
      hasDiff: false,
      changeStatus: "present",
      addedLines: 0,
      deletedLines: 0,
      oldLinesBefore: 0,
    });
  }
  rows.sort((a, b) => a.path.localeCompare(b.path));
  treeRowsCache.set(commitSha, rows);
  return rows;
}

function parseNumstat(output) {
  const map = new Map();
  const lines = output.split("\n");
  for (const line of lines) {
    if (!line) continue;
    const firstTab = line.indexOf("\t");
    if (firstTab < 0) continue;
    const secondTab = line.indexOf("\t", firstTab + 1);
    if (secondTab < 0) continue;
    const addedRaw = line.slice(0, firstTab);
    const deletedRaw = line.slice(firstTab + 1, secondTab);
    const filePath = line.slice(secondTab + 1);
    if (!isTrackableSourcePath(filePath)) continue;
    const added = addedRaw === "-" ? 0 : Number(addedRaw);
    const deleted = deletedRaw === "-" ? 0 : Number(deletedRaw);
    map.set(filePath, {
      added: Number.isFinite(added) ? added : 0,
      deleted: Number.isFinite(deleted) ? deleted : 0,
    });
  }
  return map;
}

function getDiffStats(fromSha, toSha) {
  return parseNumstat(
    gitText(["diff", "--numstat", "--no-renames", fromSha, toSha, "--", ...pathSpecs])
  );
}

function countLinesInBuffer(buffer, start, end) {
  let lines = 0;
  for (let i = start; i < end; i++) {
    if (buffer[i] === 10) lines += 1;
  }
  if (end > start && buffer[end - 1] !== 10) {
    lines += 1;
  }
  return lines;
}

function fillLineCountCacheForOids(oids) {
  const chunkSize = 220;
  for (let offset = 0; offset < oids.length; offset += chunkSize) {
    const chunk = oids.slice(offset, offset + chunkSize);
    const input = `${chunk.join("\n")}\n`;
    const output = gitBuffer(["cat-file", "--batch"], input);
    let index = 0;
    for (const oid of chunk) {
      const headerEnd = output.indexOf(10, index);
      if (headerEnd < 0) {
        lineCountCache.set(oid, 0);
        break;
      }
      const header = output.toString("utf8", index, headerEnd);
      index = headerEnd + 1;
      if (header.endsWith(" missing")) {
        lineCountCache.set(oid, 0);
        continue;
      }
      const headerParts = header.split(" ");
      const size = Number(headerParts[2]);
      if (!Number.isFinite(size) || size < 0) {
        lineCountCache.set(oid, 0);
        continue;
      }
      const contentStart = index;
      const contentEnd = index + size;
      if (contentEnd > output.length) {
        lineCountCache.set(oid, 0);
        break;
      }
      lineCountCache.set(oid, countLinesInBuffer(output, contentStart, contentEnd));
      index = contentEnd + 1;
    }
  }
}

function computeOldLineCounts(beforeMap, diffMap) {
  const missingOids = [];
  const pathToOid = new Map();
  for (const [filePath, stat] of diffMap) {
    if (!stat) continue;
    if ((stat.added || 0) + (stat.deleted || 0) <= 0) continue;
    const before = beforeMap.get(filePath);
    if (!before || !before.oid) continue;
    pathToOid.set(filePath, before.oid);
    if (!lineCountCache.has(before.oid)) {
      missingOids.push(before.oid);
    }
  }
  if (missingOids.length > 0) {
    fillLineCountCacheForOids(Array.from(new Set(missingOids)));
  }
  const oldLineCounts = new Map();
  for (const [filePath, oid] of pathToOid) {
    oldLineCounts.set(filePath, lineCountCache.get(oid) || 0);
  }
  return oldLineCounts;
}

function buildDiffPayload(fromRef, toRef, sizeBasis) {
  const fromSha = resolveCommit(fromRef);
  const toSha = resolveCommit(toRef);
  const key = `${fromSha}|${toSha}|${sizeBasis}`;
  const cacheHit = diffCache.get(key);
  if (cacheHit) return cacheHit;

  const t0 = Date.now();
  const beforeMap = getTreeForCommit(fromSha);
  const afterMap = getTreeForCommit(toSha);
  const diffMap = getDiffStats(fromSha, toSha);
  const oldLineCounts = computeOldLineCounts(beforeMap, diffMap);

  const allPaths = new Set([...beforeMap.keys(), ...afterMap.keys()]);
  const rows = [];
  for (const filePath of allPaths) {
    const before = beforeMap.get(filePath);
    const after = afterMap.get(filePath);
    const bytesBefore = before ? before.bytes : 0;
    const bytesAfter = after ? after.bytes : 0;
    const bytes = chooseLayoutBytes(bytesBefore, bytesAfter, sizeBasis);
    if (bytes <= 0) continue;

    const delta = diffMap.get(filePath) || { added: 0, deleted: 0 };
    const presentBefore = !!before;
    const presentAfter = !!after;
    const status = !presentBefore
      ? "added"
      : !presentAfter
        ? "deleted"
        : delta.added || delta.deleted
          ? "modified"
          : "unchanged";

    rows.push({
      path: filePath,
      bytes,
      bytesBefore,
      bytesAfter,
      extension: (after || before).extension,
      hasDiff: true,
      changeStatus: status,
      addedLines: delta.added,
      deletedLines: delta.deleted,
      oldLinesBefore: oldLineCounts.get(filePath) || 0,
    });
  }
  rows.sort((a, b) => a.path.localeCompare(b.path));

  const payload = {
    meta: {
      generatedAt: new Date().toISOString(),
      diff: {
        from: String(fromRef),
        to: String(toRef),
        sizeBasis,
        fromResolved: fromSha,
        toResolved: toSha,
      },
      stats: {
        rows: rows.length,
        changedFiles: rows.reduce((count, row) => {
          return row.addedLines || row.deletedLines ? count + 1 : count;
        }, 0),
        buildMs: Date.now() - t0,
      },
    },
    rows,
  };
  diffCache.set(key, payload);
  return payload;
}

function buildTreePayload(refName) {
  const resolved = resolveCommit(refName);
  const t0 = Date.now();
  const rows = getTreeRowsForCommit(resolved);
  return {
    meta: {
      generatedAt: new Date().toISOString(),
      tree: {
        ref: String(refName),
        resolved,
      },
      stats: {
        rows: rows.length,
        buildMs: Date.now() - t0,
      },
    },
    rows,
  };
}

function listRefs() {
  const tagLines = gitText(["tag", "--sort=-v:refname"]).split("\n").filter(Boolean);
  const localBranchLines = gitText(["for-each-ref", "--format=%(refname:short)", "refs/heads"])
    .split("\n")
    .filter(Boolean);
  const remoteBranchLines = gitText(["for-each-ref", "--format=%(refname:short)", "refs/remotes/origin"])
    .split("\n")
    .filter(Boolean)
    .filter((name) => name !== "origin/HEAD");
  const recentLines = gitText(["log", "--no-decorate", "--pretty=format:%H\t%s", "-n", "120"])
    .split("\n")
    .filter(Boolean);
  const head = gitText(["rev-parse", "--abbrev-ref", "HEAD"]).trim();

  const tags = tagLines.slice(0, 300);
  const branches = Array.from(new Set([...remoteBranchLines, ...localBranchLines])).slice(0, 180);
  const recentCommits = recentLines.map((line) => {
    const tab = line.indexOf("\t");
    if (tab < 0) {
      return {
        ref: line.trim(),
        subject: "",
      };
    }
    return {
      ref: line.slice(0, tab),
      subject: line.slice(tab + 1),
    };
  });
  const refs = Array.from(new Set([...tags, ...branches]));
  const from = tags.includes(defaultFrom) ? defaultFrom : tags[1] || tags[0] || "HEAD~1";
  const to = tags.includes(defaultTo) ? defaultTo : tags[0] || "HEAD";
  return {
    generatedAt: new Date().toISOString(),
    defaultFrom: from,
    defaultTo: to,
    refs,
    tags,
    branches,
    recentCommits,
    head,
  };
}

function getParentsPayload(refName) {
  const resolved = resolveCommit(refName);
  const line = gitText(["rev-list", "--parents", "-n", "1", resolved]).trim();
  const parts = line ? line.split(" ").filter(Boolean) : [];
  const parents = parts.length > 1 ? parts.slice(1) : [];
  return {
    ref: String(refName),
    resolved,
    parents,
  };
}

function resolveCommitMaybe(refName) {
  try {
    return resolveCommit(refName);
  } catch {
    return "";
  }
}

function resolveMainTarget(targetName) {
  const requested = String(targetName || "").trim();
  const candidates = [];
  if (requested) candidates.push(requested);
  if (!candidates.includes("origin/main")) candidates.push("origin/main");
  if (!candidates.includes("main")) candidates.push("main");
  for (const candidate of candidates) {
    const resolved = resolveCommitMaybe(candidate);
    if (resolved) {
      return {
        targetUsed: candidate,
        targetResolved: resolved,
      };
    }
  }
  return {
    targetUsed: "HEAD",
    targetResolved: resolveCommit("HEAD"),
  };
}

function getMergeBasePayload(refName, targetName) {
  const ref = String(refName || "").trim();
  const refResolved = resolveCommit(ref);
  const { targetUsed, targetResolved } = resolveMainTarget(targetName);
  const mergeBase = gitText(["merge-base", refResolved, targetResolved]).trim();
  if (!mergeBase) {
    throw new Error(`No merge-base found for ${ref} and ${targetUsed}`);
  }
  return {
    ref,
    refResolved,
    targetRequested: String(targetName || ""),
    targetUsed,
    targetResolved,
    mergeBase,
  };
}

function getFilePayload(refName, filePath) {
  const pathInRepo = normalizeRepoPath(filePath);
  const resolved = resolveCommit(refName);
  const spec = `${resolved}:${pathInRepo}`;
  const content = gitText(["show", spec]);
  return {
    ref: String(refName),
    resolved,
    path: pathInRepo,
    extension: extensionOf(pathInRepo),
    content,
    sizeBytes: Buffer.byteLength(content, "utf8"),
  };
}

function getFileDiffPayload(fromRef, toRef, filePath) {
  const pathInRepo = normalizeRepoPath(filePath);
  const fromResolved = resolveCommit(fromRef);
  const toResolved = resolveCommit(toRef);
  const patch = gitText([
    "diff",
    "--no-color",
    "--unified=120",
    fromResolved,
    toResolved,
    "--",
    pathInRepo,
  ]);
  return {
    from: String(fromRef),
    to: String(toRef),
    fromResolved,
    toResolved,
    path: pathInRepo,
    extension: extensionOf(pathInRepo),
    patch,
    hasDiff: patch.trim().length > 0,
  };
}

function clipForPrompt(text, maxChars) {
  const input = String(text || "");
  if (input.length <= maxChars) return input;
  return `${input.slice(0, maxChars)}\n\n[...truncated for summary...]`;
}

function parseSummaryText(text) {
  const raw = String(text || "").replace(/\r/g, "").trim();
  if (!raw) {
    return {
      oneLine: "Summary unavailable.",
      paragraph: "No summary content was returned.",
    };
  }
  const lines = raw.split("\n");
  const firstIndex = lines.findIndex((line) => line.trim().length > 0);
  if (firstIndex < 0) {
    return {
      oneLine: "Summary unavailable.",
      paragraph: "No summary content was returned.",
    };
  }
  let oneLine = lines[firstIndex].trim();
  if (oneLine.length > 220) {
    oneLine = `${oneLine.slice(0, 217)}...`;
  }
  const paragraphRaw = lines.slice(firstIndex + 1).join(" ").replace(/\s+/g, " ").trim();
  const paragraph = paragraphRaw || oneLine;
  return { oneLine, paragraph };
}

function runCodexSparkSummary(promptText) {
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "oxcaml-summary-"));
  const outputFile = path.join(tempDir, "last-message.txt");
  try {
    execFileSync(
      "codex",
      [
        "--ask-for-approval",
        "never",
        "exec",
        "-m",
        "gpt-5.3-codex-spark",
        "-c",
        "model_reasoning_effort=\"low\"",
        "--sandbox",
        "read-only",
        "--output-last-message",
        outputFile,
        promptText,
      ],
      {
        cwd: repoRoot,
        encoding: "utf8",
        maxBuffer,
      }
    );
    return fs.readFileSync(outputFile, "utf8");
  } finally {
    fs.rmSync(tempDir, { recursive: true, force: true });
  }
}

function buildFileSummaryPrompt(filePayload) {
  const preview = clipForPrompt(filePayload.content, 42_000);
  return [
    "You are summarizing a source file for a treemap viewer.",
    "Output format requirements:",
    "1) First line: a single concise one-line summary.",
    "2) Then one blank line.",
    "3) Then exactly one paragraph summary (3-5 sentences).",
    "Do not use bullets, markdown headings, code fences, or extra sections.",
    "",
    `Path: ${filePayload.path}`,
    `Ref: ${filePayload.resolved}`,
    "",
    "Source file content:",
    preview,
  ].join("\n");
}

function buildDiffSummaryPrompt(diffPayload) {
  const preview = clipForPrompt(diffPayload.patch, 42_000);
  return [
    "You are summarizing a source diff for a treemap viewer.",
    "Output format requirements:",
    "1) First line: a single concise one-line summary of the change.",
    "2) Then one blank line.",
    "3) Then exactly one paragraph summary (3-5 sentences).",
    "Focus on what changed and why it matters.",
    "Do not use bullets, markdown headings, code fences, or extra sections.",
    "",
    `Path: ${diffPayload.path}`,
    `From: ${diffPayload.fromResolved}`,
    `To: ${diffPayload.toResolved}`,
    "",
    "Unified diff:",
    preview || "(No textual diff available.)",
  ].join("\n");
}

function getSummarySpec({ refName, fromRef, toRef, filePath }) {
  const pathInRepo = normalizeRepoPath(filePath);
  const hasDiffRefs = !!fromRef || !!toRef;
  if (hasDiffRefs) {
    if (!fromRef || !toRef) {
      throw new Error("Both from and to are required for diff summaries");
    }
    const fromResolved = resolveCommit(fromRef);
    const toResolved = resolveCommit(toRef);
    return {
      mode: "diff",
      path: pathInRepo,
      from: String(fromRef),
      to: String(toRef),
      fromResolved,
      toResolved,
      summaryKey: `diff|${fromResolved}|${toResolved}|${pathInRepo}`,
    };
  }

  const ref = String(refName || "HEAD");
  const resolved = resolveCommit(ref);
  return {
    mode: "file",
    path: pathInRepo,
    ref,
    resolved,
    summaryKey: `file|${resolved}|${pathInRepo}`,
  };
}

function readSummaryRow(summaryKey) {
  const row = summarySelectStmt.get(summaryKey);
  return row || null;
}

function queueSummaryJob(spec, nowMs) {
  summaryUpsertPendingStmt.run(
    spec.summaryKey,
    spec.mode,
    spec.path,
    spec.mode === "file" ? spec.resolved : null,
    spec.mode === "diff" ? spec.fromResolved : null,
    spec.mode === "diff" ? spec.toResolved : null,
    nowMs,
    nowMs
  );
  if (summaryInFlight.has(spec.summaryKey)) return;
  const runPromise = (async () => {
    try {
      let prompt = "";
      if (spec.mode === "diff") {
        const diffPayload = getFileDiffPayload(spec.fromResolved, spec.toResolved, spec.path);
        prompt = buildDiffSummaryPrompt(diffPayload);
      } else {
        const filePayload = getFilePayload(spec.resolved, spec.path);
        prompt = buildFileSummaryPrompt(filePayload);
      }
      const raw = runCodexSparkSummary(prompt);
      const parsed = parseSummaryText(raw);
      summaryMarkReadyStmt.run(
        parsed.oneLine,
        parsed.paragraph,
        Date.now(),
        spec.summaryKey
      );
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      summaryMarkFailedStmt.run(message, Date.now(), spec.summaryKey);
    } finally {
      summaryInFlight.delete(spec.summaryKey);
    }
  })();
  summaryInFlight.set(spec.summaryKey, runPromise);
}

function getSummaryPayload({ refName, fromRef, toRef, filePath, trigger }) {
  const spec = getSummarySpec({ refName, fromRef, toRef, filePath });
  const nowMs = Date.now();
  const row = readSummaryRow(spec.summaryKey);
  if (row && row.status === "ready" && row.one_line && row.paragraph) {
    return {
      status: "ready",
      mode: spec.mode,
      path: spec.path,
      ref: spec.mode === "file" ? spec.ref : undefined,
      resolved: spec.mode === "file" ? spec.resolved : undefined,
      from: spec.mode === "diff" ? spec.from : undefined,
      to: spec.mode === "diff" ? spec.to : undefined,
      fromResolved: spec.mode === "diff" ? spec.fromResolved : undefined,
      toResolved: spec.mode === "diff" ? spec.toResolved : undefined,
      oneLine: String(row.one_line),
      paragraph: String(row.paragraph),
      cached: true,
      updatedAtMs: Number(row.updated_at_ms || 0),
    };
  }

  const startedAtMs = Number(row && row.started_at_ms ? row.started_at_ms : 0);
  const ageMs = startedAtMs > 0 ? nowMs - startedAtMs : Number.POSITIVE_INFINITY;
  const shouldStart = !!trigger && (!row || ageMs >= summaryRetryMs);
  if (shouldStart) {
    queueSummaryJob(spec, nowMs);
  }

  return {
    status: "pending",
    mode: spec.mode,
    path: spec.path,
    ref: spec.mode === "file" ? spec.ref : undefined,
    resolved: spec.mode === "file" ? spec.resolved : undefined,
    from: spec.mode === "diff" ? spec.from : undefined,
    to: spec.mode === "diff" ? spec.to : undefined,
    fromResolved: spec.mode === "diff" ? spec.fromResolved : undefined,
    toResolved: spec.mode === "diff" ? spec.toResolved : undefined,
    startedAtMs: shouldStart ? nowMs : startedAtMs,
    retryAfterMs: summaryRetryMs,
    lastError: row && row.last_error ? String(row.last_error) : "",
  };
}

function getRefsPayload() {
  const now = Date.now();
  if (refsCache && now - refsCache.timeMs < 15_000) {
    return refsCache.payload;
  }
  const payload = listRefs();
  refsCache = {
    timeMs: now,
    payload,
  };
  return payload;
}

function serveStatic(urlPath, res) {
  const decoded = decodeURIComponent(urlPath || "/");
  const relative = decoded === "/" ? "/source-treemap.html" : decoded;
  const safePath = path.resolve(webRoot, `.${relative}`);
  if (!safePath.startsWith(webRoot)) {
    sendText(res, 403, "Forbidden");
    return;
  }
  let filePath = safePath;
  if (fs.existsSync(filePath) && fs.statSync(filePath).isDirectory()) {
    filePath = path.join(filePath, "index.html");
  }
  if (!fs.existsSync(filePath) || !fs.statSync(filePath).isFile()) {
    sendText(res, 404, "Not found");
    return;
  }
  const data = fs.readFileSync(filePath);
  const ext = path.extname(filePath).toLowerCase();
  const contentType = {
    ".html": "text/html; charset=utf-8",
    ".js": "application/javascript; charset=utf-8",
    ".mjs": "application/javascript; charset=utf-8",
    ".json": "application/json; charset=utf-8",
    ".css": "text/css; charset=utf-8",
    ".svg": "image/svg+xml",
  }[ext] || "application/octet-stream";
  res.statusCode = 200;
  res.setHeader("Content-Type", contentType);
  res.setHeader("Cache-Control", "no-store");
  res.end(data);
}

function sendJson(res, statusCode, payload) {
  const body = JSON.stringify(payload);
  res.statusCode = statusCode;
  res.setHeader("Content-Type", "application/json; charset=utf-8");
  res.setHeader("Cache-Control", "no-store");
  res.end(body);
}

function sendText(res, statusCode, body) {
  res.statusCode = statusCode;
  res.setHeader("Content-Type", "text/plain; charset=utf-8");
  res.end(body);
}
