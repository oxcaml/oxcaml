(function () {
  const assetVersion = "20260421-utop-output";
  const script = document.currentScript;
  const moduleUrl = script?.dataset.moduleSrc
    ? new URL(script.dataset.moduleSrc, script.src || document.baseURI).href
    : new URL(`oxcaml-embed-module.js?v=${assetVersion}`, script?.src || document.baseURI).href;

  const load = import(moduleUrl);
  const existingApi = window.OxCamlPlayground;
  window.OxCamlPlayground = {
    ...(existingApi && typeof existingApi === "object" ? existingApi : {}),
    load,
  };

  load.catch((error) => {
    console.error("Failed to load OxCaml playground embed module", error);
  });
}());
