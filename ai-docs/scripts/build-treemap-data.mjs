#!/usr/bin/env node
"use strict";

import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";

const allowedExtensions = new Set(["ml", "mli", "mll", "mly", "c", "h", "cmm", "s", "asm"]);
const pathSpecs = Array.from(allowedExtensions, (ext) => `*.${ext}`);
const maxBuffer = 1024 * 1024 * 128;
const defaultFrom = "5.3.0";
const defaultTo = "5.4.0";

const argv = process.argv.slice(2);
const positional = [];
const options = {
  from: "",
  to: "",
  sizeBasis: "max",
  current: false,
};

for (const arg of argv) {
  if (arg.startsWith("--from=")) {
    options.from = arg.slice("--from=".length).trim();
    continue;
  }
  if (arg.startsWith("--to=")) {
    options.to = arg.slice("--to=".length).trim();
    continue;
  }
  if (arg.startsWith("--size-basis=")) {
    options.sizeBasis = arg.slice("--size-basis=".length).trim();
    continue;
  }
  if (arg === "--current") {
    options.current = true;
    continue;
  }
  positional.push(arg);
}

const repoRoot = positional[0] || process.cwd();
const outputPath =
  positional[1] || path.join(repoRoot, "ai-docs", "source-treemap-data.json");

if (!options.current && !options.from && !options.to) {
  options.from = defaultFrom;
  options.to = defaultTo;
}

if ((options.from && !options.to) || (!options.from && options.to)) {
  throw new Error("Provide both --from and --to, or neither. Use --current for non-diff mode.");
}

if (!["before", "after", "max"].includes(options.sizeBasis)) {
  throw new Error('Expected --size-basis to be one of: "before", "after", "max".');
}

const rows = options.from && options.to
  ? buildDiffRows(repoRoot, options.from, options.to, options.sizeBasis)
  : buildCurrentRows(repoRoot);

rows.sort((a, b) => String(a.path).localeCompare(String(b.path)));
const payload = {
  meta: {
    generatedAt: new Date().toISOString(),
    diff: options.from && options.to
      ? {
          from: options.from,
          to: options.to,
          sizeBasis: options.sizeBasis,
        }
      : null,
  },
  rows,
};
fs.writeFileSync(outputPath, `${JSON.stringify(payload, null, 2)}\n`, "utf8");
if (options.from && options.to) {
  console.log(
    `Generated ${rows.length} source rows to ${outputPath} (diff: ${options.from}..${options.to}, size-basis=${options.sizeBasis})`
  );
} else {
  console.log(`Generated ${rows.length} tracked source rows to ${outputPath} (current mode)`);
}

function gitText(repo, args) {
  return execFileSync("git", ["-C", repo, ...args], {
    encoding: "utf8",
    maxBuffer,
  });
}

function gitBlobText(repo, oid) {
  return execFileSync("git", ["-C", repo, "cat-file", "-p", oid], {
    encoding: "utf8",
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
  const ext = extensionOf(rel);
  return allowedExtensions.has(ext);
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

function logicalLineCount(text) {
  if (!text) return 0;
  let newlines = 0;
  for (let i = 0; i < text.length; i++) {
    if (text.charCodeAt(i) === 10) newlines += 1;
  }
  if (text.endsWith("\n")) return newlines;
  return newlines + 1;
}

function computeOldLineCounts(repoRootValue, beforeMap, diffMap) {
  const lineCounts = new Map();
  for (const [filePath, stat] of diffMap) {
    if (!stat) continue;
    if (stat.added === 0 && stat.deleted === 0) continue;
    const before = beforeMap.get(filePath);
    if (!before || !before.oid) continue;
    try {
      const text = gitBlobText(repoRootValue, before.oid);
      lineCounts.set(filePath, logicalLineCount(text));
    } catch {
      lineCounts.set(filePath, 0);
    }
  }
  return lineCounts;
}

function chooseLayoutBytes(bytesBefore, bytesAfter, sizeBasis) {
  if (sizeBasis === "before") return bytesBefore;
  if (sizeBasis === "after") return bytesAfter;
  return Math.max(bytesBefore, bytesAfter);
}

function buildDiffRows(repoRootValue, fromCommit, toCommit, sizeBasis) {
  const beforeMap = parseLsTreeRows(gitText(repoRootValue, ["ls-tree", "-r", "-l", fromCommit]));
  const afterMap = parseLsTreeRows(gitText(repoRootValue, ["ls-tree", "-r", "-l", toCommit]));
  const diffMap = parseNumstat(
    gitText(repoRootValue, [
      "diff",
      "--numstat",
      "--no-renames",
      fromCommit,
      toCommit,
      "--",
      ...pathSpecs,
    ])
  );
  const oldLineCounts = computeOldLineCounts(repoRootValue, beforeMap, diffMap);

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
  return rows;
}

function buildCurrentRows(repoRootValue) {
  const trackedFiles = gitText(repoRootValue, ["ls-files", "-z"])
    .split("\u0000")
    .map((value) => value.trim())
    .filter(Boolean)
    .filter(isTrackableSourcePath);

  const rows = [];
  for (const filePath of trackedFiles) {
    const absPath = path.join(repoRootValue, filePath);
    try {
      const stat = fs.statSync(absPath);
      if (!stat.isFile()) continue;
      rows.push({
        path: filePath,
        bytes: stat.size,
        bytesBefore: stat.size,
        bytesAfter: stat.size,
        extension: extensionOf(filePath),
        hasDiff: false,
        changeStatus: "present",
        addedLines: 0,
        deletedLines: 0,
        oldLinesBefore: 0,
      });
    } catch {
      continue;
    }
  }
  return rows;
}
