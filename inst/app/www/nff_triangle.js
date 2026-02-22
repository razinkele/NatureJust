// NatureJust-EU — NFF Triangle interactions & navigation
var NatureJust = NatureJust || {};

// Shared navigation helper — switches navbar tab by name
NatureJust.navigateTo = function(target) {
  Shiny.setInputValue('main_nav', target, {priority: 'event'});
  var navLinks = document.querySelectorAll('.navbar-nav .nav-link');
  navLinks.forEach(function(link) {
    if (link.textContent.trim().indexOf(target) !== -1) {
      link.click();
    }
  });
};

$(document).ready(function() {

  // --- Vertex click navigation with ripple ---
  $(document).on('click', '.nff-vertex', function() {
    var target = $(this).data('target');
    var svg = $(this).closest('svg')[0];
    var cx = parseFloat($(this).attr('cx'));
    var cy = parseFloat($(this).attr('cy'));

    // Create SVG ripple
    var ripple = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
    ripple.setAttribute('cx', cx);
    ripple.setAttribute('cy', cy);
    ripple.setAttribute('r', '12');
    ripple.setAttribute('fill', 'none');
    ripple.setAttribute('stroke', '#E07A5F');
    ripple.setAttribute('stroke-width', '2');
    ripple.setAttribute('opacity', '0.8');
    ripple.style.transition = 'all 0.6s cubic-bezier(0.16, 1, 0.3, 1)';
    svg.appendChild(ripple);

    requestAnimationFrame(function() {
      ripple.setAttribute('r', '45');
      ripple.setAttribute('opacity', '0');
    });
    setTimeout(function() { ripple.remove(); }, 650);

    NatureJust.navigateTo(target);
  });

  // --- Vertex hover tooltips ---
  var tooltipEl = $('<div class="nff-tooltip"></div>').appendTo('body');

  var descriptions = {
    'Spatial Equity': 'Nature for Nature \u2014 Intrinsic value of biodiversity, conservation for its own sake',
    'Scenarios': 'Nature for Society \u2014 Instrumental value, nature\u2019s contributions to human wellbeing',
    'Justice': 'Nature as Culture \u2014 Relational value, cultural and spiritual connections to nature'
  };

  $(document).on('mouseenter', '.nff-vertex', function() {
    var key = $(this).data('target');
    tooltipEl.text(descriptions[key] || key).addClass('visible');
  });

  $(document).on('mousemove', '.nff-vertex', function(e) {
    tooltipEl.css({ left: e.pageX + 14, top: e.pageY - 34 });
  });

  $(document).on('mouseleave', '.nff-vertex', function() {
    tooltipEl.removeClass('visible');
  });

});
