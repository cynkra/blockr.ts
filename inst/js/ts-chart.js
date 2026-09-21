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

  /* ---- interval bands ----------------------------------------------------
     THE CONVENTION IS IN THE ID, which is what makes it survive the trip. A
     forecast reaching this chart has been through the extractor block and
     maybe three dplyr blocks after that, and an attribute on the frame would
     have been dropped by the first of them without a word. So the bounds
     arrive as ordinary series named `<id>_lower_80` and `<id>_upper_80`, and
     this pairs them back up.

     Unpaired they stay ordinary series and draw as lines: a board that
     filters one bound away gets two lines and can see that it did, rather
     than a band that quietly stopped being one.

     uPlot fills between two series indices, so the two edges are declared at
     `width: 0` with a transparent stroke and then kept out of the legend.
     Upper first, lower second. Alpha falls as the level rises, so an 80
     inside a 95 reads as two rings rather than one flat wash. */
  var BOUND_RX = /^(.*)_(lower|upper)_(\d+(?:\.\d+)?)$/;

  function bandSpec(ids) {
    // Nested by base and then by level, so nothing has to be joined into a
    // key: a series id comes from the user's data and any separator we
    // picked would eventually turn up inside one.
    var byBase = {};
    var bound = {};

    ids.forEach(function (id, i) {
      var m = BOUND_RX.exec(id);
      if (!m) return;
      byBase[m[1]] = byBase[m[1]] || {};
      byBase[m[1]][m[3]] = byBase[m[1]][m[3]] || {};
      byBase[m[1]][m[3]][m[2]] = i;
    });

    var bands = [];
    Object.keys(byBase).forEach(function (base) {
      var at = ids.indexOf(base);
      // A band around a series that is not on the chart has nothing to be an
      // interval OF, so its bounds stay lines and say so.
      if (at < 0) return;
      Object.keys(byBase[base]).forEach(function (level) {
        var p = byBase[base][level];
        if (p.lower == null || p.upper == null) return;
        bound[p.lower] = true;
        bound[p.upper] = true;
        bands.push({base: at, level: +level, lower: p.lower, upper: p.upper});
      });
    });

    // Widest first, so a 95 is painted before the 80 that sits inside it.
    bands.sort(function (a, b) { return b.level - a.level; });

    // COLOURS ARE ASSIGNED OVER THE VISIBLE SERIES ONLY. Letting a pair of
    // bounds consume two hues shifts every series after them, so a forecast
    // and the same data without one would not be the same colour.
    var color = [];
    var next = 0;
    ids.forEach(function (id, i) {
      color[i] = bound[i] ? null : PALETTE[next++ % PALETTE.length];
    });

    return {bands: bands, bound: bound, color: color};
  }

  function rgba(hex, a) {
    var n = parseInt(hex.slice(1), 16);
    return 'rgba(' + ((n >> 16) & 255) + ',' + ((n >> 8) & 255) + ',' +
      (n & 255) + ',' + a + ')';
  }

  /* The x where the forecast starts: the first date at which any bound has a
     value. READ FROM THE DATA, never from a block argument, so a filter
     downstream moves the rule instead of leaving it pointing at nothing. */
  function splitAt(spec, data) {
    var first = null;
    spec.bands.forEach(function (b) {
      var v = data[b.lower + 1];
      for (var i = 0; i < v.length; i++) {
        if (v[i] != null) {
          if (first === null || i < first) first = i;
          return;
        }
      }
    });
    return first === null ? null : data[0][first];
  }

  /* The dashed rule at the last observation, drawn on `drawClear`, which runs
     before the series, so it never sits on top of the line it is dating. */
  function splitRule(spec, pal) {
    return {
      hooks: {
        drawClear: function (u) {
          var x = splitAt(spec, u.data);
          if (x == null) return;
          var cx = u.valToPos(x, 'x', true);
          var ctx = u.ctx;
          ctx.save();
          ctx.setLineDash([3, 3]);
          ctx.strokeStyle = pal.muted;
          ctx.lineWidth = 1;
          ctx.beginPath();
          ctx.moveTo(cx, u.bbox.top);
          ctx.lineTo(cx, u.bbox.top + u.bbox.height);
          ctx.stroke();
          ctx.restore();
        }
      }
    };
  }

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

  // THE AXES ARE DRAWN TO CANVAS, WHICH DOES NOT WARN ABOUT A MISSING FONT.
  // Asking for a family the page has not loaded silently falls through to
  // the browser default, which is how this chart spent a day in Inter-that-
  // was-actually-system-ui while the board around it was Open Sans. Read the
  // family the element has actually inherited instead of naming one, so the
  // chart follows the theme rather than guessing at it.
  function chartFont(el, px) {
    var fam = getComputedStyle(el).fontFamily;
    return px + 'px ' + (fam && fam.length ? fam : 'sans-serif');
  }

  function opts(payload, spec, width, height, pal, font) {
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
        if (spec.bound[i]) {
          // An edge of a band is not a line. It still has to be a series,
          // because that is what uPlot fills between.
          return {label: id, stroke: 'transparent', width: 0,
                  points: {show: false}};
        }
        return {
          label: id,
          stroke: spec.color[i],
          width: 1.6,
          points: {show: false}
        };
      })),
      bands: spec.bands.map(function (b) {
        return {
          series: [b.upper + 1, b.lower + 1],
          fill: rgba(spec.color[b.base], b.level >= 95 ? 0.10 : 0.18)
        };
      })
    };
  }

  /* ---- the readout ------------------------------------------------------
     Values while the cursor is in the plot, and NOTHING at rest: a resting
     value looks identical to a hovered one, and they mean different things.
     Clicking a name hides that series. */
  /* One entry for every band on the chart, naming the levels rather than
     the six series they came from. */
  function fc_band_key(spec) {
    if (!spec.bands.length) return '';
    // ONE ENTRY FOR THE LEVELS, NOT ONE PER BAND. Two series with an 80 and
    // a 95 each carry four bands and two distinct levels, and it is the
    // levels the reader needs. The swatch is grey rather than one series'
    // colour, because no single hue is true of all of them.
    var lv = spec.bands.map(function (b) { return b.level; })
      .filter(function (v, i, a) { return a.indexOf(v) === i; })
      .sort(function (a, b) { return a - b; })
      .join(' / ');
    return '<span class="blockr-ts-legend__item is-static">' +
      '<span class="blockr-ts-legend__sw blockr-ts-legend__sw--band"></span>' +
      '<span class="blockr-ts-legend__name">' + lv + '</span></span>';
  }

  function readout(el, payload, spec, u) {
    var shown = payload.ids.map(function () { return true; });
    // THE EDGES LEAVE THE LEGEND. Two entries for one uncertainty is noise,
    // and clicking one of them would hide half a band.
    var seen = payload.ids.map(function (id, i) { return i; })
      .filter(function (i) { return !spec.bound[i]; });

    function render(i) {
      var html = seen.map(function (s) {
        var val = '';
        if (i != null) {
          var v = u.data[s + 1][i];
          val = '<span class="blockr-ts-legend__val">' +
            (v == null ? '–' : fmtVal.format(v)) + '</span>';
        }
        return '<button type="button" class="blockr-ts-legend__item' +
          (shown[s] ? '' : ' is-off') + '" data-i="' + s + '">' +
          '<span class="blockr-ts-legend__sw" style="background:' +
          spec.color[s] + '"></span>' +
          '<span class="blockr-ts-legend__name"></span>' + val + '</button>';
      }).join('') + fc_band_key(spec);

      el.innerHTML = html + (i == null ? '' :
        '<span class="blockr-ts-legend__when"></span>');

      // Names and dates go in as TEXT, never interpolated into the markup
      // above: a series id comes from the user's data and can contain
      // anything, including a tag.
      var names = el.querySelectorAll('.blockr-ts-legend__name');
      seen.forEach(function (s, k) { names[k].textContent = payload.ids[s]; });
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
    var spec = bandSpec(payload.ids);
    var data = [payload.time].concat(payload.series);
    var height = Math.max(120, host.clientHeight - 26);

    var render;
    var o = opts(payload, spec, plot.clientWidth || 600, height, pal,
                 chartFont(host, 11));
    o.plugins = [
      {hooks: {setCursor: function (u) { if (render) render(u.cursor.idx); }}},
      splitRule(spec, pal)
    ];

    var u = new uPlot(o, data, plot);
    host._uplot = u;

    render = readout(legend, payload, spec, u);
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
