(async function () {
  const data = window.IKINDS_GRAPH_DATA;
  const svg = document.getElementById('ikinds-graph');
  const infoTitle = document.getElementById('info-title');
  const infoBody = document.getElementById('info-body');
  const searchInput = document.getElementById('search');
  const zoomInBtn = document.getElementById('zoom-in');
  const zoomOutBtn = document.getElementById('zoom-out');
  const zoomResetBtn = document.getElementById('zoom-reset');
  const toggleDarkBtn = document.getElementById('toggle-dark');
  const toggleInactiveBtn = document.getElementById('toggle-inactive');
  const depsList = document.getElementById('dependencies-list');
  const depsSection = document.getElementById('dependencies-section');
  const depsntsList = document.getElementById('dependents-list');
  const depsntsSection = document.getElementById('dependents-section');

  if (!data || !svg) {
    console.error('Graph data or SVG element missing');
    return;
  }

  if (typeof ELK === 'undefined') {
    console.error('ELK layout engine not loaded');
    return;
  }

  const elk = new ELK();
  const arrowsPointForward = false;
  const baseNodeWidth = 230;
  const baseNodeHeight = 58;
  const margin = 40;

  const inactiveNodes = new Set(
    (data.nodes || [])
      .filter((node) => node.inactive)
      .map((node) => node.id)
  );

  let hideInactive = true;
  let currentSearchQuery = '';

  let zoomLevel = 1;
  let panX = 0;
  let panY = 0;
  let isDragging = false;
  let dragStartX = 0;
  let dragStartY = 0;

  let mainGroup = null;
  let nodeGroup = null;
  let edgeGroup = null;

  let nodeElements = new Map();
  let edgeElements = new Map();
  let adjacency = new Map();
  let nodeDeps = new Map();
  let nodeDepnts = new Map();
  let rootedNodes = new Set();
  let visibleNodes = new Map();

  const defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
  const marker = document.createElementNS('http://www.w3.org/2000/svg', 'marker');
  marker.setAttribute('id', 'arrowhead');
  marker.setAttribute('viewBox', '0 0 10 10');
  marker.setAttribute('refX', '9');
  marker.setAttribute('refY', '5');
  marker.setAttribute('markerUnits', 'strokeWidth');
  marker.setAttribute('markerWidth', '6');
  marker.setAttribute('markerHeight', '5');
  marker.setAttribute('orient', 'auto');
  const markerPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  markerPath.setAttribute('d', 'M 0 0 L 10 5 L 0 10 z');
  markerPath.setAttribute('fill', 'context-stroke');
  markerPath.setAttribute('stroke', 'none');
  marker.appendChild(markerPath);
  defs.appendChild(marker);
  svg.appendChild(defs);

  function offsetX(x) {
    return margin + x;
  }

  function offsetY(y) {
    return margin + y;
  }

  function applyTransform() {
    if (!mainGroup) return;
    mainGroup.setAttribute(
      'transform',
      `translate(${panX}, ${panY}) scale(${zoomLevel})`
    );
  }

  function registerEdgeAdjacency(source, target, edgeId) {
    if (!adjacency.has(source)) adjacency.set(source, new Set());
    if (!adjacency.has(target)) adjacency.set(target, new Set());
    adjacency.get(source).add(edgeId);
    adjacency.get(target).add(edgeId);
  }

  function clearClasses() {
    nodeElements.forEach((el) => el.classList.remove('highlight', 'active'));
    edgeElements.forEach((el) => el.classList.remove('highlight'));
  }

  function setInfo(title, body) {
    infoTitle.textContent = title;
    infoBody.textContent = body || '';
  }

  function clearFocus() {
    clearClasses();
    depsSection.style.display = 'none';
    depsntsSection.style.display = 'none';
    depsList.replaceChildren();
    depsntsList.replaceChildren();
    setInfo(
      'Graph controls',
      'Use the toolbar to zoom, reset, or toggle unused Ikinds modules. Hover nodes or edges to view dependency details, or type in the search box to filter by filename.'
    );
  }

  function focusNode(nodeId) {
    clearClasses();
    const nodeEl = nodeElements.get(nodeId);
    if (!nodeEl) return;
    nodeEl.classList.add('highlight', 'active');

    const edges = adjacency.get(nodeId) || [];
    edges.forEach((edgeId) => {
      const edgeEl = edgeElements.get(edgeId);
      if (!edgeEl) return;
      edgeEl.classList.add('highlight');
      const otherId =
        edgeEl.dataset.source === nodeId
          ? edgeEl.dataset.target
          : edgeEl.dataset.source;
      const otherNode = nodeElements.get(otherId);
      if (otherNode) otherNode.classList.add('highlight');
    });

    const info = nodeEl.dataset.info || 'No additional details available.';
    const label = visibleNodes.get(nodeId)?.label || nodeId;
    setInfo(label, info);

    const deps = nodeDeps.get(nodeId) || [];
    const depnts = nodeDepnts.get(nodeId) || [];

    if (deps.length > 0) {
      depsSection.style.display = 'block';
      const fragment = document.createDocumentFragment();
      deps.forEach((dep) => {
        const button = document.createElement('button');
        button.type = 'button';
        button.dataset.node = dep;
        button.textContent = dep;
        button.addEventListener('click', () => focusNode(dep));
        fragment.appendChild(button);
      });
      depsList.replaceChildren(fragment);
    } else {
      depsSection.style.display = 'none';
      depsList.replaceChildren();
    }

    if (depnts.length > 0) {
      depsntsSection.style.display = 'block';
      const fragment = document.createDocumentFragment();
      depnts.forEach((dep) => {
        const button = document.createElement('button');
        button.type = 'button';
        button.dataset.node = dep;
        button.textContent = dep;
        button.addEventListener('click', () => focusNode(dep));
        fragment.appendChild(button);
      });
      depsntsList.replaceChildren(fragment);
    } else {
      depsntsSection.style.display = 'none';
      depsntsList.replaceChildren();
    }
  }

  function focusEdge(edgeId) {
    clearClasses();
    const edgeEl = edgeElements.get(edgeId);
    if (!edgeEl) return;
    edgeEl.classList.add('highlight');
    const srcNode = nodeElements.get(edgeEl.dataset.source);
    const dstNode = nodeElements.get(edgeEl.dataset.target);
    if (srcNode) srcNode.classList.add('highlight');
    if (dstNode) dstNode.classList.add('highlight');

    const info = edgeEl.dataset.info || 'Dependency edge';
    const title = arrowsPointForward
      ? `${edgeEl.dataset.source} → ${edgeEl.dataset.target}`
      : `${edgeEl.dataset.source} ← ${edgeEl.dataset.target}`;
    setInfo(title, info);
  }

  function applySearchFilter(query) {
    currentSearchQuery = query;
    const q = query.trim().toLowerCase();
    if (!q) {
      nodeElements.forEach((el) => el.classList.remove('hidden'));
      edgeElements.forEach((el) => el.classList.remove('hidden'));
      return;
    }

    const matches = new Set();
    nodeElements.forEach((_, id) => {
      const node = visibleNodes.get(id);
      const haystack = `${id} ${(node?.label || '')}`.toLowerCase();
      if (haystack.includes(q)) matches.add(id);
    });

    nodeElements.forEach((el, id) => {
      el.classList.toggle('hidden', !matches.has(id));
    });

    edgeElements.forEach((el) => {
      const src = el.dataset.source;
      const tgt = el.dataset.target;
      const visible = matches.has(src) && matches.has(tgt);
      el.classList.toggle('hidden', !visible);
    });
  }

  async function renderGraph() {
    const filteredNodes = hideInactive
      ? data.nodes.filter((node) => !inactiveNodes.has(node.id))
      : data.nodes.slice();
    const activeNodeIds = new Set(filteredNodes.map((node) => node.id));
    const filteredEdges = data.edges.filter(
      (edge) =>
        activeNodeIds.has(edge.source) && activeNodeIds.has(edge.target)
    );

    nodeElements = new Map();
    edgeElements = new Map();
    adjacency = new Map();
    nodeDeps = new Map();
    nodeDepnts = new Map();
    rootedNodes = new Set();
    visibleNodes = new Map();

    filteredNodes.forEach((node) => {
      nodeDeps.set(node.id, []);
      nodeDepnts.set(node.id, []);
      visibleNodes.set(node.id, node);
    });

    filteredEdges.forEach((edge) => {
      nodeDeps.get(edge.source)?.push(edge.target);
      nodeDepnts.get(edge.target)?.push(edge.source);
    });

    const memo = new Map();
    function getTransitiveDependencies(nodeId) {
      if (memo.has(nodeId)) return memo.get(nodeId);
      const visited = new Set([nodeId]);
      const deps = nodeDeps.get(nodeId) || [];
      deps.forEach((dep) => {
        getTransitiveDependencies(dep).forEach((v) => visited.add(v));
      });
      memo.set(nodeId, visited);
      return visited;
    }

    filteredNodes.forEach((node) => {
      if (!node.id.startsWith('typing/ikinds/')) {
        getTransitiveDependencies(node.id).forEach((id) =>
          rootedNodes.add(id)
        );
      }
    });

    const elkGraph = {
      id: 'ikinds-graph',
      layoutOptions: {
        'elk.algorithm': 'layered',
        'elk.direction': 'UP',
        'elk.layered.unnecessaryBendpoints': 'true',
        'elk.layered.spacing.nodeNodeBetweenLayers': '80',
        'elk.spacing.nodeNode': '32',
        'elk.spacing.componentComponent': '80',
        'elk.margin': `[top=${margin},left=${margin},bottom=${margin},right=${margin}]`
      },
      children: filteredNodes.map((node) => ({
        id: node.id,
        width: Math.max(baseNodeWidth, node.label.length * 7),
        height: baseNodeHeight
      })),
      edges: filteredEdges.map((edge) => ({
        id: `${edge.source}→${edge.target}`,
        sources: [edge.source],
        targets: [edge.target]
      }))
    };

    let layout;
    try {
      layout = await elk.layout(elkGraph);
    } catch (err) {
      console.error('Failed to compute layout with ELK', err);
      setInfo('Layout error', 'Could not compute layout. See console for details.');
      return;
    }

    const width = layout.width + margin * 2;
    const height = layout.height + margin * 2;
    svg.setAttribute('viewBox', `0 0 ${width} ${height}`);

    if (mainGroup) {
      svg.removeChild(mainGroup);
    }

    mainGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    edgeGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    nodeGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    mainGroup.appendChild(edgeGroup);
    mainGroup.appendChild(nodeGroup);
    svg.appendChild(mainGroup);

    const childLookup = new Map();
    layout.children?.forEach((child) => {
      childLookup.set(child.id, child);
    });

    const layoutEdgeLookup = new Map();
    layout.edges?.forEach((edge) => {
      layoutEdgeLookup.set(edge.id, edge);
    });

    filteredEdges.forEach((edge) => {
      const layoutEdge = layoutEdgeLookup.get(`${edge.source}→${edge.target}`);
      if (!layoutEdge) return;

      const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
      const isRooted =
        rootedNodes.has(edge.source) && rootedNodes.has(edge.target);
      const classes = ['edge'];
      if (!isRooted) classes.push('unrooted');
      if (inactiveNodes.has(edge.source) || inactiveNodes.has(edge.target)) {
        classes.push('inactive');
      }
      group.setAttribute('class', classes.join(' '));
      const edgeId = `${edge.source}→${edge.target}`;
      group.dataset.edgeId = edgeId;
      group.dataset.source = edge.source;
      group.dataset.target = edge.target;
      if (edge.info) group.dataset.info = edge.info;

      const polyline = document.createElementNS('http://www.w3.org/2000/svg', 'polyline');
      polyline.setAttribute('class', 'edge-main');
      polyline.setAttribute('stroke-linecap', 'round');

      const section = layoutEdge.sections?.[0];
      if (section) {
        const rawPoints = [
          section.startPoint,
          ...(section.bendPoints || []),
          section.endPoint
        ];
        const drawPoints = arrowsPointForward
          ? rawPoints
          : rawPoints.slice().reverse();
        const pointsAttr = drawPoints
          .map((pt) => `${offsetX(pt.x)},${offsetY(pt.y)}`)
          .join(' ');

        const hitPolyline = document.createElementNS('http://www.w3.org/2000/svg', 'polyline');
        hitPolyline.setAttribute('points', pointsAttr);
        hitPolyline.setAttribute('class', 'edge-hit');
        hitPolyline.setAttribute('stroke-linecap', 'round');
        hitPolyline.setAttribute('pointer-events', 'stroke');
        group.appendChild(hitPolyline);

        polyline.setAttribute('points', pointsAttr);
        polyline.setAttribute('marker-end', 'url(#arrowhead)');
        group.appendChild(polyline);
        edgeGroup.appendChild(group);
        edgeElements.set(edgeId, group);
        registerEdgeAdjacency(edge.source, edge.target, edgeId);

        group.addEventListener('mouseenter', () => focusEdge(edgeId));
        group.addEventListener('mouseleave', clearFocus);
      }
    });

    filteredNodes.forEach((node) => {
      const layoutNode = childLookup.get(node.id);
      if (!layoutNode) return;

      const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
      const isRooted = rootedNodes.has(node.id);
      const classes = ['node'];
      if (!isRooted) classes.push('unrooted');
      if (node.id.startsWith('typing/ikinds/')) classes.push('ikind');
      if (inactiveNodes.has(node.id)) classes.push('inactive');
      group.setAttribute('class', classes.join(' '));
      group.dataset.nodeId = node.id;
      if (node.info) group.dataset.info = node.info;

      const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
      rect.setAttribute('x', offsetX(layoutNode.x));
      rect.setAttribute('y', offsetY(layoutNode.y));
      rect.setAttribute('width', layoutNode.width);
      rect.setAttribute('height', layoutNode.height);

      const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
      text.setAttribute('x', offsetX(layoutNode.x) + layoutNode.width / 2);
      text.setAttribute('y', offsetY(layoutNode.y) + layoutNode.height / 2);
      text.setAttribute('text-anchor', 'middle');
      text.textContent = node.label;

      group.appendChild(rect);
      group.appendChild(text);
      nodeGroup.appendChild(group);
      nodeElements.set(node.id, group);

      group.addEventListener('mouseenter', () => focusNode(node.id));
      group.addEventListener('mouseleave', clearFocus);
    });

    zoomLevel = 1;
    panX = 0;
    panY = 0;
    applyTransform();

    applySearchFilter(currentSearchQuery);
    clearFocus();
  }

  function handleZoom(delta) {
    const nextZoom = Math.min(Math.max(zoomLevel + delta, 0.2), 2.5);
    zoomLevel = nextZoom;
    applyTransform();
  }

  zoomInBtn.addEventListener('click', () => handleZoom(0.1));
  zoomOutBtn.addEventListener('click', () => handleZoom(-0.1));
  zoomResetBtn.addEventListener('click', () => {
    zoomLevel = 1;
    panX = 0;
    panY = 0;
    applyTransform();
  });

  svg.addEventListener('wheel', (e) => {
    e.preventDefault();
    handleZoom(e.deltaY < 0 ? 0.1 : -0.1);
  });

  svg.addEventListener('mousedown', (e) => {
    isDragging = true;
    dragStartX = e.clientX - panX;
    dragStartY = e.clientY - panY;
  });

  svg.addEventListener('mousemove', (e) => {
    if (!isDragging) return;
    panX = e.clientX - dragStartX;
    panY = e.clientY - dragStartY;
    applyTransform();
  });

  svg.addEventListener('mouseup', () => {
    isDragging = false;
  });

  svg.addEventListener('mouseleave', () => {
    isDragging = false;
  });

  searchInput.addEventListener('input', (e) => {
    applySearchFilter(e.target.value || '');
  });

  toggleDarkBtn.addEventListener('click', () => {
    const body = document.documentElement;
    const isDark = body.getAttribute('data-theme') === 'dark';
    body.setAttribute('data-theme', isDark ? '' : 'dark');
  });

  toggleInactiveBtn?.addEventListener('click', async () => {
    hideInactive = !hideInactive;
    toggleInactiveBtn.setAttribute('aria-pressed', hideInactive ? 'true' : 'false');
    toggleInactiveBtn.classList.toggle('active', hideInactive);
    toggleInactiveBtn.textContent = hideInactive ? 'Show unused' : 'Hide unused';
    toggleInactiveBtn.title = hideInactive
      ? 'Show unused Ikinds modules'
      : 'Hide unused Ikinds modules';
    await renderGraph();
  });

  await renderGraph();
})();
