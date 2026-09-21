/* Mount the chart in a headless DOM and read back what it asked uPlot for.
 *
 * `inst/js/ts-chart.js` is the half of this package R tests cannot reach.
 * The band pairing in particular is pure client logic: R emits a long frame
 * whose ids happen to end in `_lower_80`, and everything that turns those
 * into one shaded band rather than two stray lines happens here. A browser
 * run proves it once; this proves it on every change.
 *
 * UPLOT IS STUBBED, THE CHART IS NOT. The stub captures the options object
 * and hands back the smallest instance `mount()` uses, so the code under
 * test is the real `bandSpec()` -> `opts()` -> legend path. The alternative,
 * a real uPlot, needs a canvas happy-dom does not have, and would test
 * uPlot's drawing rather than our pairing.
 *
 * Usage:
 *   const c = mount(['m', 'm_lower_80', 'm_upper_80'], 24);
 *   assert.equal(c.opts.bands.length, 1);
 *   c.close();
 */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { Window } = require('happy-dom');

const JS_DIR = path.join(__dirname, '..', '..', 'inst', 'js');
const read = (f) => fs.readFileSync(path.join(JS_DIR, f), 'utf8');

/** The palette ts-chart.js assigns from, in order. */
const PALETTE = [
  '#2563eb', '#e8833a', '#3fa66a', '#d4546a',
  '#8b5cf6', '#0e9aa7', '#b08900', '#6b7280'
];

/* A series that is a straight line over `n` points, so a test can say which
 * points are missing without also having to invent values. `null` is what R
 * sends for a gap (`na = "null"`), and it is the only thing the band logic
 * looks at. */
const line = (n, from = 0) =>
  Array.from({ length: n }, (_, i) => (i < from ? null : 100 + i));

/**
 * Build the payload R's `ts_chart_html()` emits: columnar, one array for the
 * time and one per series, time in unix SECONDS.
 *
 * @param {string[]} ids
 * @param {number} n points per series
 * @param {Object<string, number>} [forecastFrom] id -> first index with a
 *   value, for the bound series that only exist over the horizon.
 */
function payload(ids, n = 24, forecastFrom = {}) {
  return {
    ids: ids.slice(),
    time: Array.from({ length: n }, (_, i) => 1000000 + i * 86400),
    series: ids.map((id) => line(n, forecastFrom[id] || 0))
  };
}

/** The ids a forecast of `base` carries at these levels, extractor order. */
const forecastIds = (base, levels = [80, 95]) =>
  [base].concat(...levels.map((l) => [`${base}_lower_${l}`, `${base}_upper_${l}`]));

/**
 * Mount the chart over a payload and return what it handed uPlot.
 *
 * @returns {{opts: any, data: any[], legend: string, names: string[],
 *            window: any, close: function}}
 */
function mount(ids, n = 24, forecastFrom = {}) {
  const win = new Window({ url: 'http://localhost' });
  const doc = win.document;

  const captured = { opts: null, data: null };

  /* The instance `mount()` touches: `data` for the readout, `cursor.idx` for
   * the hover, and the four methods it wires to events. `valToPos`, `ctx`
   * and `bbox` exist for the split rule's draw hook, which the tests call
   * directly rather than waiting for a paint that never comes. */
  const strokes = [];
  win.uPlot = function (opts, data, el) {
    captured.opts = opts;
    captured.data = data;
    const inst = {
      data: data,
      cursor: { idx: null },
      bbox: { top: 0, height: 100 },
      ctx: {
        save() {}, restore() {}, beginPath() {}, stroke() { strokes.push(this._x); },
        moveTo(x) { this._x = x; }, lineTo() {}, setLineDash() {},
        strokeStyle: '', lineWidth: 1
      },
      valToPos: (v) => v,
      setSeries() {}, setScale() {}, setSize() {}, destroy() {}
    };
    el.appendChild(doc.createElement('canvas'));
    return inst;
  };

  const host = doc.createElement('div');
  host.id = 'chart';
  host.setAttribute('data-ts', JSON.stringify(payload(ids, n, forecastFrom)));
  doc.body.appendChild(host);

  win.eval(read('ts-chart.js'));
  win.BlockrTsChart.mount('chart', null);

  const legendEl = host.querySelector('.blockr-ts-legend');

  return {
    opts: captured.opts,
    data: captured.data,
    window: win,
    /* Serialized, because an array built inside the window's realm has that
     * realm's prototypes and `deepStrictEqual` counts that as a difference.
     * JSON is also the honest comparison: it is what crosses the wire. */
    get bands() {
      return JSON.parse(JSON.stringify(captured.opts.bands));
    },
    /** Text of every legend entry, in order. */
    get names() {
      return Array.from(legendEl.querySelectorAll('.blockr-ts-legend__name'))
        .map((e) => e.textContent);
    },
    /** The interval key, or null when there is no band on the chart. */
    get bandKey() {
      const sw = legendEl.querySelector('.blockr-ts-legend__sw--band');
      return sw ? sw.nextElementSibling.textContent : null;
    },
    /** Where the split rule was drawn, in x units (valToPos is identity). */
    splitX() {
      const rule = captured.opts.plugins.find((p) => p.hooks.drawClear);
      strokes.length = 0;
      rule.hooks.drawClear({
        data: captured.data,
        valToPos: (v) => v,
        ctx: {
          save() {}, restore() {}, beginPath() {},
          stroke() { strokes.push(this._x); },
          moveTo(x) { this._x = x; }, lineTo() {}, setLineDash() {},
          strokeStyle: '', lineWidth: 1
        },
        bbox: { top: 0, height: 100 }
      });
      return strokes.length ? strokes[0] : null;
    },
    close() { win.close(); }
  };
}

module.exports = { mount, payload, forecastIds, PALETTE, line };
