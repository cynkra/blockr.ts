/*
 * blockr.ts -- the time series chart.
 *
 * One entry point, `BlockrTsChart.mount(id, toggleId)`, called by the script
 * the R side emits next to the container. The container carries its data in
 * `data-ts`, so there is no message round trip and no output binding:
 * renderUI puts a fresh div on the page and this fills it.
 *
 * uPlot rather than ECharts, decided 2026-09-21. 22 KB against 363 KB
 * gzipped, and under 2 ms to draw where ECharts took 8.7 ms with the same
 * columnar payload. The two features that looked like they would keep
 * ECharts, a shaded confidence band and dated annotations, turned out to be
 * native (`bands`) and twenty-four lines of canvas respectively. See
 * `_scratch/ts-charts/`.
 *
 * WHAT UPLOT DOES NOT GIVE YOU is chrome: no legend worth using at this size,
 * no tooltip. The readout below is ours, and that is the standing cost of the
 * choice.
 */
(function () {
  'use strict';

  // Enough hues to tell a handful of series apart, ordered so the first two
  // are the pair that matters most often (a series and its adjusted version)
  // and are distinguishable in the common forms of colour blindness.
  var PALETTE = [
    '#2563eb', '#e8833a', '#3fa66a', '#d4546a',
    '#8b5cf6', '#0e9aa7', '#b08900', '#6b7280'
  ];

  var fmtVal = new Intl.NumberFormat(undefined, {maximumFractionDigits: 3});
  var fmtDay = new Intl.DateTimeFormat(undefined,
    {year: 'numeric', month: 'short', day: '2-digit'});

  function readPayload(el) {
    try {
      return JSON.parse(el.getAttribute('data-ts'));
    } catch (e) {
      return null;
    }
  }

  function cssVar(name, fallback) {
    var v = getComputedStyle(document.documentElement).getPropertyValue(name);
    v = (v || '').trim();
    return v.length ? v : fallback;
  }

  // Light or dark is the page's business, not ours: read the colours it
  // actually uses, so the chart follows a theme switch without being told.
  function palette() {
    return {
      ink: cssVar('--bs-body-color', '#3c3c43'),
      muted: cssVar('--bs-secondary-color', '#67676c'),
      grid: 'rgba(128,128,135,.18)'
    };
  }

  function opts(payload, width, height, pal) {
    var font = '11px Inter, system-ui, sans-serif';
    return {
      width: width,
      height: height,
      padding: [12, 10, 0, 0],
      cursor: {
        // Sideways only. A y-drag zoom on a preview is a way to get lost.
        drag: {x: true, y: false},
        points: {size: 6}
      },
      scales: {x: {time: true}, y: {auto: true}},
      axes: [
        {stroke: pal.muted, grid: {show: false},
         ticks: {stroke: pal.grid, size: 4}, font: font},
        // NEVER FORCE ZERO on a level series. An index that moves between 98
        // and 104 is a flat line on a zero-based axis, and the movement is
        // the entire reason the chart is on the screen. uPlot's `auto` scale
        // does the right thing here; there is no zero to turn off.
        {stroke: pal.muted, grid: {stroke: pal.grid, width: 1, dash: [3, 3]},
         ticks: {show: false}, font: font}
      ],
      legend: {show: false},
      series: [{}].concat(payload.ids.map(function (id, i) {
        return {
          label: id,
          stroke: PALETTE[i % PALETTE.length],
          width: 1.6,
          points: {show: false}
        };
      }))
    };
  }

  /* ---- the readout ------------------------------------------------------
     Values while the cursor is in the plot, and NOTHING at rest: a resting
     value looks identical to a hovered one, and they mean different things.
     Clicking a name hides that series. */
  function readout(el, payload, u) {
    var shown = payload.ids.map(function () { return true; });

    function render(i) {
      var html = payload.ids.map(function (id, s) {
        var val = '';
        if (i != null) {
          var v = u.data[s + 1][i];
          val = '<span class="blockr-ts-legend__val">' +
            (v == null ? '–' : fmtVal.format(v)) + '</span>';
        }
        return '<button type="button" class="blockr-ts-legend__item' +
          (shown[s] ? '' : ' is-off') + '" data-i="' + s + '">' +
          '<span class="blockr-ts-legend__sw" style="background:' +
          PALETTE[s % PALETTE.length] + '"></span>' +
          '<span class="blockr-ts-legend__name"></span>' + val + '</button>';
      }).join('');

      el.innerHTML = html + (i == null ? '' :
        '<span class="blockr-ts-legend__when"></span>');

      // Names and dates go in as TEXT, never interpolated into the markup
      // above: a series id comes from the user's data and can contain
      // anything, including a tag.
      var names = el.querySelectorAll('.blockr-ts-legend__name');
      payload.ids.forEach(function (id, s) { names[s].textContent = id; });
      if (i != null) {
        el.querySelector('.blockr-ts-legend__when').textContent =
          fmtDay.format(new Date(u.data[0][i] * 1000));
      }
    }

    el.addEventListener('click', function (ev) {
      var b = ev.target.closest('.blockr-ts-legend__item');
      if (!b || !el.contains(b)) return;
      var i = +b.dataset.i;
      shown[i] = !shown[i];
      u.setSeries(i + 1, {show: shown[i]});
      render(u.cursor.idx);
    });

    return render;
  }

  function mount(id, toggleId) {
    var host = document.getElementById(id);
    if (!host || typeof uPlot === 'undefined') return;

    var payload = readPayload(host);
    if (!payload || !payload.ids || !payload.ids.length) return;

    // A dock panel that is hidden and shown again can hand back the same
    // node, and a second uPlot on it would stack a canvas on top of the old
    // one. The instance is parked on the element so it can be destroyed.
    if (host._uplot) {
      host._uplot.destroy();
      host._uplot = null;
    }
    host.innerHTML = '';

    var plot = document.createElement('div');
    plot.className = 'blockr-ts-plot';
    var legend = document.createElement('div');
    legend.className = 'blockr-ts-legend';
    host.appendChild(plot);
    host.appendChild(legend);

    var pal = palette();
    var data = [payload.time].concat(payload.series);
    var height = Math.max(120, host.clientHeight - 26);

    var render;
    var o = opts(payload, plot.clientWidth || 600, height, pal);
    o.plugins = [{
      hooks: {setCursor: function (u) { if (render) render(u.cursor.idx); }}
    }];

    var u = new uPlot(o, data, plot);
    host._uplot = u;

    render = readout(legend, payload, u);
    render(null);

    plot.addEventListener('dblclick', function () {
      u.setScale('x', {min: null, max: null});
    });
    plot.addEventListener('mouseleave', function () { render(null); });

    // The chart being on the page is what makes the chart/table switch
    // meaningful, so revealing it here needs no message and cannot race the
    // block's own markup.
    if (toggleId && window.BlockrTsView) {
      window.BlockrTsView.offer(toggleId);
    }

    // A dock panel is resized by its neighbours, not by the window, so a
    // window listener is not enough. The observer dies with the element.
    if (window.ResizeObserver) {
      var ro = new ResizeObserver(function () {
        if (!document.body.contains(host)) { ro.disconnect(); return; }
        u.setSize({
          width: plot.clientWidth,
          height: Math.max(120, host.clientHeight - 26)
        });
      });
      ro.observe(host);
    }
  }

  window.BlockrTsChart = { mount: mount };
})();
