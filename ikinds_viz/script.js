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

  let zoomLevel = 1;
  let panX = 0;
  let panY = 0;
  let isDragging = false;
  let dragStartX = 0;
  let dragStartY = 0;

  const baseNodeWidth = 230;
  const baseNodeHeight = 58;

  const edgeMeta = new Map();
  const nodeDeps = new Map();
  const nodeDepnts = new Map();

  data.nodes.forEach(node => {
    nodeDeps.set(node.id, []);
    nodeDepnts.set(node.id, []);
  });

  data.edges.forEach((edge) => {
    const id = `${edge.source}→${edge.target}`;
    edgeMeta.set(id, edge);
    nodeDeps.get(edge.source)?.push(edge.target);
    nodeDepnts.get(edge.target)?.push(edge.source);
  });

  function getTransitiveDependencies(nodeId, visited = new Set()) {
    if (visited.has(nodeId)) return visited;
    visited.add(nodeId);
    const deps = nodeDeps.get(nodeId) || [];
    deps.forEach(dep => getTransitiveDependencies(dep, visited));
    return visited;
  }

  const rootedNodes = new Set();
  data.nodes.forEach(node => {
    if (!node.id.startsWith('typing/ikinds/')) {
      const reachable = getTransitiveDependencies(node.id);
      reachable.forEach(id => rootedNodes.add(id));
    }
  });

  const elkGraph = {
    id: 'ikinds-graph',
    layoutOptions: {
      'elk.algorithm': 'layered',
      'elk.direction': 'RIGHT',
      'elk.layered.unnecessaryBendpoints': 'true',
      'elk.layered.spacing.nodeNodeBetweenLayers': '80',
      'elk.spacing.nodeNode': '32',
      'elk.spacing.componentComponent': '80',
      'elk.margin': '[top=20,left=20,bottom=20,right=20]'
    },
    children: data.nodes.map((node) => ({
      id: node.id,
      width: Math.max(baseNodeWidth, node.label.length * 7),
      height: baseNodeHeight
    })),
    edges: data.edges.map((edge, index) => ({
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
    infoTitle.textContent = 'Layout error';
    infoBody.textContent = 'Could not compute layout with ELK. See console for details.';
    return;
  }

  const margin = 40;
  const width = layout.width + margin * 2;
  const height = layout.height + margin * 2;
  svg.setAttribute('viewBox', `0 0 ${width} ${height}`);

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

  const mainGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const nodeGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const edgeGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const labelGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  labelGroup.setAttribute('class', 'edge-labels');

  mainGroup.appendChild(edgeGroup);
  mainGroup.appendChild(labelGroup);
  mainGroup.appendChild(nodeGroup);
  svg.appendChild(mainGroup);

  const nodeElements = new Map();
  const edgeElements = new Map();
  const adjacency = new Map();

  function offsetX(x) {
    return margin + x;
  }
  function offsetY(y) {
    return margin + y;
  }

  function registerEdgeAdjacency(source, target, edgeId) {
    if (!adjacency.has(source)) adjacency.set(source, new Set());
    if (!adjacency.has(target)) adjacency.set(target, new Set());
    adjacency.get(source).add(edgeId);
    adjacency.get(target).add(edgeId);
  }

  const childLookup = new Map();
  layout.children?.forEach((child) => {
    childLookup.set(child.id, child);
  });

  layout.edges?.forEach((edge) => {
    const meta = edgeMeta.get(edge.id);
    if (!meta) return;

    const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const isRooted = rootedNodes.has(meta.source) && rootedNodes.has(meta.target);
    const classes = ['edge'];
    if (meta.style === 'dashed') classes.push('dashed');
    if (!isRooted) classes.push('unrooted');
    group.setAttribute('class', classes.join(' '));
    group.dataset.edgeId = edge.id;
    group.dataset.source = meta.source;
    group.dataset.target = meta.target;
    if (meta.info) group.dataset.info = meta.info;
    if (meta.label) group.dataset.label = meta.label;

    const polyline = document.createElementNS('http://www.w3.org/2000/svg', 'polyline');
    polyline.setAttribute('class', 'edge-main');
    polyline.setAttribute('stroke-linecap', 'round');

    const section = edge.sections?.[0];
    if (section) {
      const rawPoints = [section.startPoint, ...(section.bendPoints || []), section.endPoint];
      const drawPoints = arrowsPointForward ? rawPoints : rawPoints.slice().reverse();
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
      edgeElements.set(edge.id, group);
      registerEdgeAdjacency(meta.source, meta.target, edge.id);

      if (meta.label) {
        const label = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        label.setAttribute('class', 'edge-label');
        label.setAttribute('text-anchor', 'middle');
        label.setAttribute('font-size', '11');
        label.setAttribute('fill', 'var(--edge-dashed)');

        const labelPoints = rawPoints;
        const midIndex = Math.floor(labelPoints.length / 2);
        const midpoint = labelPoints[midIndex];
        const prevPt = labelPoints[Math.max(0, midIndex - 1)];
        const nextPt = labelPoints[Math.min(labelPoints.length - 1, midIndex + 1)];
        const dx = nextPt.x - prevPt.x;
        const dy = nextPt.y - prevPt.y;
        const len = Math.hypot(dx, dy) || 1;
        const offsetDist = 14;
        const offsetXLocal = midpoint.x - (dy / len) * offsetDist;
        const offsetYLocal = midpoint.y + (dx / len) * offsetDist;

        label.setAttribute('x', offsetX(offsetXLocal));
        label.setAttribute('y', offsetY(offsetYLocal));
        label.textContent = meta.label;

        labelGroup.appendChild(label);
        const bbox = label.getBBox();
        labelGroup.removeChild(label);
        const paddingX = 6;
        const paddingY = 4;
        const bg = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        bg.setAttribute('class', 'edge-label-bg');
        bg.setAttribute('x', bbox.x - paddingX);
        bg.setAttribute('y', bbox.y - paddingY);
        bg.setAttribute('width', bbox.width + paddingX * 2);
        bg.setAttribute('height', bbox.height + paddingY * 2);
        bg.setAttribute('rx', '4');
        bg.setAttribute('ry', '4');
        bg.setAttribute('fill', '#ffffff');
        bg.setAttribute('fill-opacity', '0.92');
        bg.setAttribute('stroke', '#94a3b8');
        bg.setAttribute('stroke-opacity', '0.4');
        bg.setAttribute('stroke-width', '0.5');

        const labelContainer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        labelContainer.setAttribute('class', 'edge-label-hit');
        labelContainer.dataset.edgeId = edge.id;
        labelContainer.dataset.source = meta.source;
        labelContainer.dataset.target = meta.target;
        if (meta.info) labelContainer.dataset.info = meta.info;
        if (meta.label) labelContainer.dataset.label = meta.label;
        labelContainer.appendChild(bg);
        labelContainer.appendChild(label);
        labelGroup.appendChild(labelContainer);

        labelContainer.addEventListener('mouseenter', () => focusEdge(edge.id));
        labelContainer.addEventListener('mouseleave', clearFocus);
      }
    }

    group.addEventListener('mouseenter', () => focusEdge(edge.id));
    group.addEventListener('mouseleave', clearFocus);
  });

  data.nodes.forEach((node) => {
    const layoutNode = childLookup.get(node.id);
    if (!layoutNode) return;

    const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const isRooted = rootedNodes.has(node.id);
    group.setAttribute('class', `node ${!isRooted ? 'unrooted' : ''}`);
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

    if (node.loc !== undefined) {
      const locText = document.createElementNS('http://www.w3.org/2000/svg', 'text');
      locText.setAttribute('x', offsetX(layoutNode.x) + layoutNode.width / 2);
      locText.setAttribute('y', offsetY(layoutNode.y) + layoutNode.height / 2 + 16);
      locText.setAttribute('text-anchor', 'middle');
      locText.setAttribute('font-size', '11');
      locText.setAttribute('fill', '#64748b');
      if (node.loc_mli !== undefined) {
        locText.textContent = `${node.loc} + ${node.loc_mli} LOC`;
      } else {
        locText.textContent = `${node.loc} LOC`;
      }
      group.appendChild(locText);
    }

    nodeGroup.appendChild(group);
    nodeElements.set(node.id, group);

    group.addEventListener('mouseenter', () => focusNode(node.id));
    group.addEventListener('mouseleave', clearFocus);
  });

  function setInfo(title, body) {
    infoTitle.textContent = title;
    infoBody.textContent = body || '';
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
      const otherId = edgeEl.dataset.source === nodeId ? edgeEl.dataset.target : edgeEl.dataset.source;
      const otherNode = nodeElements.get(otherId);
      if (otherNode) otherNode.classList.add('highlight');
    });

    const info = nodeEl.dataset.info || 'No additional details available.';
    setInfo(nodeId, info);

    const deps = nodeDeps.get(nodeId) || [];
    const depnts = nodeDepnts.get(nodeId) || [];

    if (deps.length > 0) {
      depsSection.style.display = 'block';
      depsList.innerHTML = deps.map(d => `<div data-node="${d}">${d}</div>`).join('');
      depsList.querySelectorAll('div').forEach(el => {
        el.addEventListener('click', () => focusNode(el.dataset.node));
      });
    } else {
      depsSection.style.display = 'none';
    }

    if (depnts.length > 0) {
      depsntsSection.style.display = 'block';
      depsntsList.innerHTML = depnts.map(d => `<div data-node="${d}">${d}</div>`).join('');
      depsntsList.querySelectorAll('div').forEach(el => {
        el.addEventListener('click', () => focusNode(el.dataset.node));
      });
    } else {
      depsntsSection.style.display = 'none';
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

    const label = edgeEl.dataset.label;
    const info = edgeEl.dataset.info || 'Dependency edge';
    const parts = arrowsPointForward
      ? [`${edgeEl.dataset.source} → ${edgeEl.dataset.target}`]
      : [`${edgeEl.dataset.source} ← ${edgeEl.dataset.target}`];
    if (label) {
      parts[0] += ` (${label})`;
    }
    const title = parts[0];
    setInfo(title, info);
  }

  function clearClasses() {
    nodeElements.forEach((el) => el.classList.remove('highlight', 'active'));
    edgeElements.forEach((el) => el.classList.remove('highlight'));
  }

  function clearFocus() {
    clearClasses();
    setInfo(
      'Ikinds dependency graph',
      'Hover nodes or edges to inspect how ikinds modules relate. Arrows point from a dependency toward the module that relies on it. Dashed edges capture hacks such as Obj.magic bridges or identity environments.'
    );
    depsSection.style.display = 'none';
    depsntsSection.style.display = 'none';
  }

  function applyTransform() {
    const mainGroup = svg.querySelector('g');
    if (mainGroup) {
      mainGroup.style.transform = `translate(${panX}px, ${panY}px) scale(${zoomLevel})`;
      mainGroup.style.transformOrigin = '0 0';
    }
  }

  function handleZoom(delta) {
    const oldZoom = zoomLevel;
    zoomLevel = Math.max(0.25, Math.min(4, zoomLevel + delta));
    const rect = svg.getBoundingClientRect();
    const centerX = rect.width / 2;
    const centerY = rect.height / 2;
    panX = centerX - (centerX - panX) * (zoomLevel / oldZoom);
    panY = centerY - (centerY - panY) * (zoomLevel / oldZoom);
    applyTransform();
  }

  zoomInBtn.addEventListener('click', () => handleZoom(0.2));
  zoomOutBtn.addEventListener('click', () => handleZoom(-0.2));
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
    const query = e.target.value.toLowerCase();
    if (!query) {
      nodeElements.forEach(el => el.classList.remove('hidden'));
      edgeElements.forEach(el => el.classList.remove('hidden'));
      return;
    }

    const matches = new Set();
    data.nodes.forEach(node => {
      if (node.id.toLowerCase().includes(query) ||
          node.label.toLowerCase().includes(query)) {
        matches.add(node.id);
      }
    });

    nodeElements.forEach((el, id) => {
      if (matches.has(id)) {
        el.classList.remove('hidden');
      } else {
        el.classList.add('hidden');
      }
    });

    edgeElements.forEach((el, id) => {
      const src = el.dataset.source;
      const tgt = el.dataset.target;
      if (matches.has(src) && matches.has(tgt)) {
        el.classList.remove('hidden');
      } else {
        el.classList.add('hidden');
      }
    });
  });

  toggleDarkBtn.addEventListener('click', () => {
    const body = document.documentElement;
    const isDark = body.getAttribute('data-theme') === 'dark';
    body.setAttribute('data-theme', isDark ? '' : 'dark');
  });

  clearFocus();
})();
