// NatureJust-EU — NFF Triangle Interactions
// Canonical NFF visual (Pereira et al. 2020; Durán et al. 2023)
// 3-colour barycentric gradient, 6 narrative markers, interactive positioning.

var NatureJust = NatureJust || {};

/* ──────────────── Navigation helper ──────────────── */

NatureJust.navigateTo = function(target) {
  // Navigate by clicking the matching navbar link (bslib::page_navbar)
  document.querySelectorAll('.navbar-nav .nav-link').forEach(function(link) {
    if (link.textContent.trim().indexOf(target) !== -1) link.click();
  });
};

// IIFE wraps internal helpers to avoid global namespace pollution.
// Only NatureJust (above) is intentionally global.
(function() {
'use strict';

/* ──────────────── Triangle geometry ──────────────── */

var TRI = {
  A: {x: 200, y: 35},   // Top — Nature for Nature
  B: {x: 365, y: 335},  // Bottom-right — Nature for Society
  C: {x: 35,  y: 335}   // Bottom-left — Nature as Culture
};

// Barycentric coordinates for point (px, py) in triangle ABC
function baryCoords(px, py) {
  var d = (TRI.B.y - TRI.C.y) * (TRI.A.x - TRI.C.x) +
          (TRI.C.x - TRI.B.x) * (TRI.A.y - TRI.C.y);
  var a = ((TRI.B.y - TRI.C.y) * (px - TRI.C.x) +
           (TRI.C.x - TRI.B.x) * (py - TRI.C.y)) / d;
  var b = ((TRI.C.y - TRI.A.y) * (px - TRI.C.x) +
           (TRI.A.x - TRI.C.x) * (py - TRI.C.y)) / d;
  return {NfN: a, NfS: b, NaC: 1 - a - b};
}

function insideTriangle(px, py) {
  var b = baryCoords(px, py);
  return b.NfN >= -0.01 && b.NfS >= -0.01 && b.NaC >= -0.01;
}

function baryToXY(a, b, c) {
  return {
    x: a * TRI.A.x + b * TRI.B.x + c * TRI.C.x,
    y: a * TRI.A.y + b * TRI.B.y + c * TRI.C.y
  };
}

// Clamp barycentric coords to valid range and normalise to sum=1
function clampBary(bary) {
  var a = Math.max(0, bary.NfN);
  var b = Math.max(0, bary.NfS);
  var c = Math.max(0, bary.NaC);
  var s = a + b + c;
  if (s === 0) return {NfN: 1/3, NfS: 1/3, NaC: 1/3};
  return {NfN: a/s, NfS: b/s, NaC: c/s};
}

/* ──────────────── 6 Illustrative Narratives (Durán et al. 2023) ──────────────── */
/* Default values — overridden at runtime by server-sent 'set-narratives' message
   to maintain a single source of truth (narratives.json). */

var NARRATIVES = {
  arcology: {
    name: 'Arcology',
    pos:  'Nature for Nature corner',
    bary: [1, 0, 0],
    desc: 'High seas as marine protected areas; comprehensive no-take zones; ' +
          'biodiversity recovery prioritized over all extraction.',
    gov:  'Strict preservation'
  },
  sharing: {
    name: 'Sharing through Sparing',
    pos:  'NfN\u2013NfS edge',
    bary: [0.5, 0.5, 0],
    desc: 'Limited bycatch quotas; rebuilt fish stocks; marine spatial planning ' +
          'balances conservation zones with sustainable harvest areas.',
    gov:  'Conservation-oriented management'
  },
  optimizing: {
    name: 'Optimizing Nature',
    pos:  'Nature for Society corner',
    bary: [0, 1, 0],
    desc: 'All aquatic systems optimized for human benefit; precision extraction ' +
          'technologies; maximum sustainable yield targets.',
    gov:  'Techno-optimist management'
  },
  commons: {
    name: 'Innovative Commons',
    pos:  'NfS\u2013NaC edge',
    bary: [0, 0.5, 0.5],
    desc: 'Community-based fisheries management; ecological restoration through ' +
          'combined traditional and modern techniques.',
    gov:  'Participatory co-management'
  },
  stewardship: {
    name: 'Reciprocal Stewardship',
    pos:  'Nature as Culture corner',
    bary: [0, 0, 1],
    desc: 'Small-scale artisanal fisheries; traditional aquaculture systems; ' +
          'cultural seascapes maintained through indigenous governance.',
    gov:  'Cultural heritage governance'
  },
  dynamic: {
    name: 'Dynamic Natures',
    pos:  'NaC\u2013NfN edge',
    bary: [0.5, 0, 0.5],
    desc: 'Traditional fishing rights respected; ecosystem-based management ' +
          'informed by indigenous ecological knowledge.',
    gov:  'Biocultural conservation'
  }
};

/* ──────────────── SVG coordinate conversion ──────────────── */

function svgPoint(svg, evt) {
  var rect = svg.getBoundingClientRect();
  var vb   = svg.viewBox.baseVal;
  return {
    x: (evt.clientX - rect.left) / rect.width  * vb.width  + vb.x,
    y: (evt.clientY - rect.top)  / rect.height * vb.height + vb.y
  };
}

function makeSVG(tag, attrs) {
  var el = document.createElementNS('http://www.w3.org/2000/svg', tag);
  for (var k in attrs) el.setAttribute(k, attrs[k]);
  return el;
}

// Round 3 fractions to integers summing to 100
function roundTo100(bary) {
  var raw = [bary.NfN * 100, bary.NfS * 100, bary.NaC * 100];
  var floored = raw.map(Math.floor);
  var remainder = 100 - floored.reduce(function(acc, val) { return acc + val; }, 0);
  var fracs = raw.map(function(v, i) { return {i: i, f: v - floored[i]}; });
  fracs.sort(function(x, y) { return y.f - x.f; });
  for (var i = 0; i < remainder; i++) floored[fracs[i].i]++;
  return {NfN: floored[0], NfS: floored[1], NaC: floored[2]};
}

/* ──────────────── Widget registry ──────────────── */

// All initialized widgets keyed by inputId
var NFF_WIDGETS = {};

// Shared drag state — only one widget can be dragged at a time
var dragState = { active: false, widget: null };

/* ──────────────── Per-widget initialisation ──────────────── */

$(document).ready(function() {
  $('.nff-triangle-widget').each(function() { initTriangle(this); });
});

function initTriangle(container) {
  var $ctr    = $(container);
  var inputId = $ctr.data('input-id');
  var svg     = container.querySelector('.nff-svg');
  if (!svg) return;

  /* ---- Position marker (appended to SVG) ---- */
  var centroid = baryToXY(1/3, 1/3, 1/3);

  var posRing = makeSVG('circle', {
    cx: centroid.x, cy: centroid.y, r: 14,
    'class': 'nff-position-ring'
  });
  var posMarker = makeSVG('circle', {
    cx: centroid.x, cy: centroid.y, r: 8,
    'class': 'nff-position-marker'
  });
  svg.appendChild(posRing);
  svg.appendChild(posMarker);

  /* ---- Tooltips (shared across widgets to avoid DOM bloat) ---- */
  var $narrativeTip = $('.nff-narrative-tooltip');
  if (!$narrativeTip.length) $narrativeTip = $('<div class="nff-narrative-tooltip"></div>').appendTo('body');
  var $vertexTip = $('.nff-tooltip');
  if (!$vertexTip.length) $vertexTip = $('<div class="nff-tooltip"></div>').appendTo('body');

  /* ---- Readout element ---- */
  var $readout = $ctr.siblings('.nff-weight-readout').first();
  if (!$readout.length) $readout = $ctr.find('.nff-weight-readout');

  /* ────── Core state update ────── */

  function updatePosition(bary, notify) {
    bary = clampBary(bary);
    var xy = baryToXY(bary.NfN, bary.NfS, bary.NaC);

    posMarker.setAttribute('cx', xy.x);
    posMarker.setAttribute('cy', xy.y);
    posRing.setAttribute('cx', xy.x);
    posRing.setAttribute('cy', xy.y);

    renderReadout(bary);

    if (notify && typeof Shiny !== 'undefined' &&
        typeof Shiny.setInputValue === 'function' && inputId) {
      Shiny.setInputValue(inputId, {
        NfN: Math.round(bary.NfN * 100),
        NfS: Math.round(bary.NfS * 100),
        NaC: Math.round(bary.NaC * 100)
      });
    }
  }

  function renderReadout(bary) {
    if (!$readout.length) return;
    var vals = roundTo100(bary);
    function mkWeight(cls, color, label, pct) {
      return $('<span class="nff-weight ' + cls + '">')
        .append($('<span class="nff-w-dot">').css('background', color))
        .append(document.createTextNode(label + '\u2002' + pct + '%'));
    }
    $readout.empty()
      .append(mkWeight('nff-w-nfn', '#0E7C7B', 'NfN', vals.NfN))
      .append(mkWeight('nff-w-nfs', '#2A6F97', 'NfS', vals.NfS))
      .append(mkWeight('nff-w-nac', '#E07A5F', 'NaC', vals.NaC));
  }

  /* ---- Initialise at centroid ---- */
  updatePosition({NfN: 1/3, NfS: 1/3, NaC: 1/3}, false);

  /* ---- Register widget for cross-module sync ---- */
  NFF_WIDGETS[inputId] = {
    svg: svg,
    posMarker: posMarker,
    posRing: posRing,
    updatePosition: updatePosition
  };

  /* ────── Click inside triangle → move position ────── */

  $(svg).on('click', function(e) {
    if ($(e.target).closest('.nff-vertex, .nff-narrative-hit').length) return;
    var pt = svgPoint(svg, e);
    if (!insideTriangle(pt.x, pt.y)) return;
    var bary = baryCoords(pt.x, pt.y);
    updatePosition(bary, true);
  });

  /* ────── Drag position marker (start only — move/end handled globally) ────── */

  $(posMarker).add(posRing).on('mousedown touchstart', function(e) {
    e.preventDefault();
    e.stopPropagation();
    dragState.active = true;
    dragState.widget = NFF_WIDGETS[inputId];
    svg.classList.add('nff-dragging');
  });

  /* ────── Vertex click → ripple + navigate ────── */

  $(svg).on('click', '.nff-vertex', function(e) {
    e.stopPropagation();
    var target = $(this).data('target');
    var cx = parseFloat(this.getAttribute('cx'));
    var cy = parseFloat(this.getAttribute('cy'));

    var ripple = makeSVG('circle', {
      cx: cx, cy: cy, r: 12,
      fill: 'none', stroke: '#E07A5F',
      'stroke-width': 2, opacity: 0.8
    });
    ripple.style.transition = 'all 0.6s cubic-bezier(0.16, 1, 0.3, 1)';
    svg.appendChild(ripple);

    requestAnimationFrame(function() {
      ripple.setAttribute('r', '45');
      ripple.setAttribute('opacity', '0');
    });
    setTimeout(function() { ripple.remove(); }, 650);

    NatureJust.navigateTo(target);
  });

  /* ────── Vertex hover tooltip ────── */

  var vertexDesc = {
    'Spatial Equity':
      'Nature for Nature \u2014 Intrinsic value of biodiversity; conservation for its own sake; the right of all species to thrive.',
    'Scenarios':
      'Nature for Society \u2014 Instrumental value; nature\u2019s contributions to human wellbeing, livelihoods, and economies.',
    'Justice':
      'Nature as Culture \u2014 Relational value; cultural and spiritual connections between people and the living world.'
  };

  $(svg).on('mouseenter', '.nff-vertex', function() {
    $vertexTip.text(vertexDesc[$(this).data('target')] || '').addClass('visible');
  }).on('mousemove', '.nff-vertex', function(e) {
    $vertexTip.css({left: e.pageX + 14, top: e.pageY - 34});
  }).on('mouseleave', '.nff-vertex', function() {
    $vertexTip.removeClass('visible');
  });

  /* ────── Narrative marker hover / click ────── */

  $(svg).on('mouseenter', '.nff-narrative-hit', function() {
    var n = NARRATIVES[$(this).data('narrative')];
    if (!n) return;
    $narrativeTip.empty()
      .append($('<div class="nff-nt-name">').text(n.name))
      .append($('<div class="nff-nt-pos">').text(n.pos))
      .append($('<div class="nff-nt-desc">').text(n.desc))
      .append($('<div class="nff-nt-gov">').append($('<strong>').text('Governance: ')).append(document.createTextNode(n.gov)))
      .append($('<div class="nff-nt-hint">').text('Double-click for full details'))
      .addClass('visible');
    svg.querySelector('.nff-narrative-marker[data-narrative="' +
      $(this).data('narrative') + '"]').classList.add('active');
  });

  $(svg).on('mousemove', '.nff-narrative-hit', function(e) {
    var winW = $(window).width();
    var ttW  = $narrativeTip.outerWidth();
    var left = e.pageX + 14;
    if (left + ttW > winW - 20) left = e.pageX - ttW - 14;
    $narrativeTip.css({left: left, top: e.pageY - 10});
  });

  $(svg).on('mouseleave', '.nff-narrative-hit', function() {
    $narrativeTip.removeClass('visible');
    $(svg).find('.nff-narrative-marker').removeClass('active');
  });

  $(svg).on('click', '.nff-narrative-hit', function(e) {
    e.stopPropagation();
    var n = NARRATIVES[$(this).data('narrative')];
    if (!n) return;
    updatePosition({NfN: n.bary[0], NfS: n.bary[1], NaC: n.bary[2]}, true);
  });

  /* ────── Keyboard support for interactive elements ────── */

  $(svg).on('keydown', '.nff-vertex', function(e) {
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault();
      $(this).trigger('click');
    }
  });

  /* ────── Narrative marker double-click → navigate to Narratives tab ────── */

  $(svg).on('dblclick', '.nff-narrative-hit', function(e) {
    e.stopPropagation();
    var narrativeId = $(this).data('narrative');
    if (typeof Shiny !== 'undefined' && typeof Shiny.setInputValue === 'function') {
      Shiny.setInputValue('navigate_to_narrative', narrativeId, {priority: 'event'});
    }
  });

  /* ────── Keyboard support for narrative markers ────── */

  $(svg).on('keydown', '.nff-narrative-hit', function(e) {
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault();
      var narrativeId = $(this).data('narrative');
      if (typeof Shiny !== 'undefined' && typeof Shiny.setInputValue === 'function') {
        Shiny.setInputValue('navigate_to_narrative', narrativeId, {priority: 'event'});
      }
    }
  });
}

/* ──────────────── Global drag handlers (single set for all widgets) ──────────────── */

$(document).on('mousemove touchmove', function(e) {
  if (!dragState.active || !dragState.widget) return;
  e.preventDefault();
  var evt = e.originalEvent.touches ? e.originalEvent.touches[0] : e;
  var pt  = svgPoint(dragState.widget.svg, evt);
  if (insideTriangle(pt.x, pt.y)) {
    dragState.widget.updatePosition(baryCoords(pt.x, pt.y), true);
  }
});

$(document).on('mouseup touchend', function() {
  if (dragState.active && dragState.widget) {
    dragState.widget.svg.classList.remove('nff-dragging');
    dragState.active = false;
    dragState.widget = null;
  }
});

/* ──────────────── Global Shiny message handlers ──────────────── */

$(document).ready(function() {
  if (typeof Shiny !== 'undefined') {
    // Send initial values for all widgets once Shiny is ready
    $(document).one('shiny:connected', function() {
      Object.keys(NFF_WIDGETS).forEach(function(id) {
        NFF_WIDGETS[id].updatePosition({NfN: 1/3, NfS: 1/3, NaC: 1/3}, true);
      });
    });

    // Broadcast weight updates to ALL triangle widgets
    Shiny.addCustomMessageHandler('nff-update-position', function(msg) {
      Object.keys(NFF_WIDGETS).forEach(function(id) {
        var w = NFF_WIDGETS[id];
        // Avoid feedback loop: only update if significantly different
        var cur = baryCoords(
          parseFloat(w.posMarker.getAttribute('cx')),
          parseFloat(w.posMarker.getAttribute('cy'))
        );
        cur = clampBary(cur);
        var dNfN = Math.abs(cur.NfN - msg.NfN / 100);
        var dNfS = Math.abs(cur.NfS - msg.NfS / 100);
        var dNaC = Math.abs(cur.NaC - msg.NaC / 100);
        if (dNfN + dNfS + dNaC > 0.02) {
          w.updatePosition({
            NfN: msg.NfN / 100,
            NfS: msg.NfS / 100,
            NaC: msg.NaC / 100
          }, false);  // false = don't send back to Shiny (avoid loop)
        }
      });
    });

    // Generic tab navigation handler (used by mod_narratives "Run as Scenario")
    Shiny.addCustomMessageHandler('nj-nav-select', function(msg) {
      NatureJust.navigateTo(msg.tab);
    });

    /* ────── Stakeholder dot rendering ────── */

    var GROUP_COLORS = {
      'Government':                '#1B4965',
      'Industry':                  '#5FA8D3',
      'Civil Society':             '#41ae76',
      'Academia':                  '#9467bd',
      'Indigenous/Local Community': '#E07A5F',
      'NGO':                       '#F2CC8F',
      'Other':                     '#9C9587'
    };

    Shiny.addCustomMessageHandler('stakeholder-add-dot', function(msg) {
      var xy = baryToXY(msg.NfN, msg.NfS, msg.NaC);
      var color = GROUP_COLORS[msg.group] || '#666';

      // Find the stakeholder triangle's SVG (any .nff-stakeholder-mode SVG)
      var svgEl = document.querySelector('.nff-stakeholder-mode .nff-svg');
      if (!svgEl) return;

      var ring = makeSVG('circle', {
        cx: xy.x, cy: xy.y, r: 9,
        fill: 'none', stroke: color, 'stroke-width': 2,
        opacity: 0.5, 'class': 'stakeholder-dot-ring'
      });
      var dot = makeSVG('circle', {
        cx: xy.x, cy: xy.y, r: 5,
        fill: color, opacity: 0.85, 'class': 'stakeholder-dot'
      });
      var label = makeSVG('text', {
        x: xy.x + 12, y: xy.y + 4,
        'font-size': '9', fill: color, opacity: 0.7,
        'class': 'stakeholder-label'
      });
      label.textContent = msg.name.substring(0, 14);

      svgEl.appendChild(ring);
      svgEl.appendChild(dot);
      svgEl.appendChild(label);
    });

    Shiny.addCustomMessageHandler('stakeholder-clear-dots', function(_msg) {
      var svgEl = document.querySelector('.nff-stakeholder-mode .nff-svg');
      if (!svgEl) return;
      $(svgEl).find('.stakeholder-dot, .stakeholder-dot-ring, .stakeholder-label').remove();
    });

    /* ────── Pathway rendering ────── */

    var pathwayElements = [];
    var currentPathway = null;

    Shiny.addCustomMessageHandler('pathway-draw', function(msg) {
      var svgEl = document.querySelector('.nff-pathway-mode .nff-svg');
      if (!svgEl) return;

      var startXY = baryToXY(msg.now_NfN, msg.now_NfS, msg.now_NaC);
      var endXY   = baryToXY(msg.future_NfN, msg.future_NfS, msg.future_NaC);

      var path = makeSVG('line', {
        x1: startXY.x, y1: startXY.y,
        x2: endXY.x, y2: endXY.y,
        stroke: '#1B4965', 'stroke-width': 2.5,
        'stroke-dasharray': '6,4', opacity: 0.7,
        'class': 'pathway-line'
      });
      var nowMarker = makeSVG('rect', {
        x: startXY.x - 6, y: startXY.y - 6,
        width: 12, height: 12,
        fill: '#d9534f', stroke: '#fff', 'stroke-width': 1.5,
        rx: 2, 'class': 'pathway-now'
      });
      var futureMarker = makeSVG('circle', {
        cx: endXY.x, cy: endXY.y, r: 8,
        fill: '#41ae76', stroke: '#fff', 'stroke-width': 1.5,
        'class': 'pathway-future'
      });
      var nowLabel = makeSVG('text', {
        x: startXY.x, y: startXY.y - 12,
        'text-anchor': 'middle', 'font-size': '10',
        fill: '#d9534f', 'class': 'pathway-label'
      });
      nowLabel.textContent = 'Now';
      var futureLabel = makeSVG('text', {
        x: endXY.x, y: endXY.y - 14,
        'text-anchor': 'middle', 'font-size': '10',
        fill: '#41ae76', 'class': 'pathway-label'
      });
      futureLabel.textContent = 'Future';
      var animDot = makeSVG('circle', {
        cx: startXY.x, cy: startXY.y, r: 6,
        fill: '#F2CC8F', stroke: '#1B4965', 'stroke-width': 1.5,
        'class': 'pathway-anim-dot'
      });

      var els = [path, nowMarker, futureMarker, nowLabel, futureLabel, animDot];
      els.forEach(function(el) { svgEl.appendChild(el); });
      pathwayElements = pathwayElements.concat(els);
      currentPathway = { start: startXY, end: endXY, animDot: animDot };
    });

    Shiny.addCustomMessageHandler('pathway-animate', function(msg) {
      if (!currentPathway) return;
      var t = Math.max(0, Math.min(1, msg.progress));
      var x = currentPathway.start.x + t * (currentPathway.end.x - currentPathway.start.x);
      var y = currentPathway.start.y + t * (currentPathway.end.y - currentPathway.start.y);
      currentPathway.animDot.setAttribute('cx', x);
      currentPathway.animDot.setAttribute('cy', y);
    });

    Shiny.addCustomMessageHandler('pathway-clear', function(_msg) {
      pathwayElements.forEach(function(el) { el.remove(); });
      pathwayElements = [];
      currentPathway = null;
    });

    /* ──────────────── Narrative page highlight ──────────────── */
    Shiny.addCustomMessageHandler('narrative-highlight', function(msg) {
      var svgEl = document.querySelector('.nff-narrative-mode .nff-svg');
      if (!svgEl) return;
      // Dim all narrative markers
      $(svgEl).find('.nff-narrative-marker')
        .css({opacity: 0.25})
        .removeClass('narrative-active');
      // Highlight selected
      var sel = $(svgEl).find('.nff-narrative-marker[data-narrative="' + msg.id + '"]');
      sel.css({opacity: 1}).addClass('narrative-active');
    });

    /* ──────────────── Server-side narrative data injection ──────────────── */
    Shiny.addCustomMessageHandler('set-narratives', function(data) {
      if (data && typeof data === 'object') {
        NARRATIVES = data;
      }
    });
  }
});

})(); // end IIFE
