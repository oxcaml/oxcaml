
function latticeEscapeHtml(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

function latticeSpan(cls, text) {
  return '<span class="' + cls + '">' + latticeEscapeHtml(text) + "</span>";
}

function latticeHighlight(text, language) {
  const ocamlKeywords = new Set([
    "and", "as", "begin", "class", "constraint", "do", "done", "downto",
    "else", "end", "exception", "external", "false", "for", "fun",
    "function", "functor", "if", "in", "include", "inherit", "initializer",
    "lazy", "let", "match", "method", "module", "mutable", "new", "nonrec",
    "object", "of", "open", "or", "private", "rec", "sig", "struct", "then",
    "to", "true", "try", "type", "val", "virtual", "when", "while", "with"
  ]);
  const latticeKeywords = new Set(["op"]);
  const tokenRe =
    /(\(\*[\s\S]*?\*\)|"(?:\\.|[^"\\])*"|\b\d+\b|\b[A-Z][A-Za-z0-9_']*\b|\b[a-z_][A-Za-z0-9_']*\b|->|<=|>=|<>|&&|\|\||[<>=|^:+\-*/]+|[][(){};,])/g;
  let out = "";
  let last = 0;
  text.replace(tokenRe, function (token, _m, offset) {
    out += latticeEscapeHtml(text.slice(last, offset));
    let cls = null;
    if (token.startsWith("(*")) cls = "tok-comment";
    else if (token.startsWith('"')) cls = "tok-string";
    else if (/^\d+$/.test(token)) cls = "tok-number";
    else if (/^[<>=|^:+\-*/]+$/.test(token) || /^(->|<=|>=|<>|&&|\|\|)$/.test(token)) cls = "tok-op";
    else if (/^[\[\](){};,]$/.test(token)) cls = "tok-punct";
    else if (/^[A-Z]/.test(token)) {
      cls = language === "lattice" ? "tok-ident" : "tok-ctor";
    } else if (language === "ocaml" && ocamlKeywords.has(token)) {
      cls = "tok-keyword";
    } else if (language === "lattice" && latticeKeywords.has(token)) {
      cls = "tok-keyword";
    } else {
      cls = language === "ocaml" ? "tok-ident" : "tok-type";
    }
    out += latticeSpan(cls, token);
    last = offset + token.length;
    return token;
  });
  out += latticeEscapeHtml(text.slice(last));
  return out;
}

function latticeEnhanceSource(node, language, title, meta) {
  const text = node.textContent.replace(/^\n+|\n+\s*$/g, "");
  node.classList.add("lattice-source", "lattice-source--" + language);
  node.innerHTML =
    '<div class="lattice-source__header">' +
      '<span class="lattice-source__title">' + latticeEscapeHtml(title) + "</span>" +
      '<span class="lattice-source__meta">' + latticeEscapeHtml(meta) + "</span>" +
    "</div>" +
    '<pre class="lattice-source__code">' + latticeHighlight(text, language) + "</pre>";
}

function latticeEnhancePanels() {
  document.querySelectorAll(".lattice-observable__panel").forEach(function (node) {
    const panel = node.getAttribute("data-lattice-panel");
    const language = panel === "ml" || panel === "mli" || panel === "test" ? "ocaml" : "text";
    if (language === "text") return;
    node.innerHTML = latticeHighlight(node.textContent, language);
  });
}

function latticeEnhanceSources() {
  document.querySelectorAll("lat").forEach(function (node) {
    latticeEnhanceSource(node, "lattice", "Lattice DSL", "source");
  });
  document.querySelectorAll("ocaml").forEach(function (node) {
    latticeEnhanceSource(node, "ocaml", "OCaml snippet", "source");
  });
}

document.addEventListener("DOMContentLoaded", function () {
  latticeEnhanceSources();
  latticeEnhancePanels();
});

document.addEventListener("click", function (event) {
  const button = event.target.closest("[data-lattice-tab]");
  if (!button) return;
  const root = button.closest(".lattice-observable");
  if (!root) return;
  const tab = button.getAttribute("data-lattice-tab");
  root.querySelectorAll("[data-lattice-tab]").forEach(function (node) {
    node.classList.toggle("is-active", node === button);
  });
  root.querySelectorAll("[data-lattice-panel]").forEach(function (node) {
    node.classList.toggle(
      "is-active",
      node.getAttribute("data-lattice-panel") === tab
    );
  });
});
