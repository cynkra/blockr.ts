/* The interval convention, client side.
 *
 * R writes `<id>_lower_80` / `<id>_upper_80` as ordinary series and the chart
 * pairs them into a band. Everything below is about that pairing holding up
 * when the frame arrives in a shape nobody drew on the whiteboard: a bound
 * without its partner, a bound without its base, two series each with two
 * levels, an id that merely looks like a bound.
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const { mount, forecastIds, PALETTE } = require('./harness');

test('a plain frame has no bands and one legend entry per series', (t) => {
  const c = mount(['mdeaths', 'fdeaths']);
  t.after(() => c.close());

  assert.deepStrictEqual(c.bands, []);
  assert.deepStrictEqual(c.names, ['mdeaths', 'fdeaths']);
  assert.equal(c.bandKey, null);
  assert.equal(c.opts.series[1].stroke, PALETTE[0]);
  assert.equal(c.opts.series[2].stroke, PALETTE[1]);
});

test('a forecast pairs its bounds, widest band first', (t) => {
  const c = mount(forecastIds('m'), 24, {
    m_lower_80: 20, m_upper_80: 20, m_lower_95: 20, m_upper_95: 20
  });
  t.after(() => c.close());

  // ids: m(0) m_lower_80(1) m_upper_80(2) m_lower_95(3) m_upper_95(4),
  // uPlot series indices are one higher.
  assert.deepStrictEqual(
    c.bands.map((b) => b.series),
    [[5, 4], [3, 2]],
    'upper first, lower second; 95 before 80 so the wider is painted under'
  );
});

test('an edge of a band is not a line', (t) => {
  const c = mount(forecastIds('m'), 24, {
    m_lower_80: 20, m_upper_80: 20, m_lower_95: 20, m_upper_95: 20
  });
  t.after(() => c.close());

  for (const i of [2, 3, 4, 5]) {
    assert.equal(c.opts.series[i].width, 0);
    assert.equal(c.opts.series[i].stroke, 'transparent');
  }
  assert.equal(c.opts.series[1].width, 1.6);
});

test('the edges leave the legend and the levels arrive as one key', (t) => {
  const c = mount(forecastIds('m'), 24, {
    m_lower_80: 20, m_upper_80: 20, m_lower_95: 20, m_upper_95: 20
  });
  t.after(() => c.close());

  assert.deepStrictEqual(c.names, ['m', '80 / 95']);
  assert.equal(c.bandKey, '80 / 95',
    'one entry naming the levels, not one per band');
});

test('two series with two levels each still name two levels', (t) => {
  const ids = forecastIds('m').concat(forecastIds('f'));
  const from = {};
  ids.slice(1).forEach((id) => { if (/_/.test(id)) from[id] = 20; });
  const c = mount(ids, 24, from);
  t.after(() => c.close());

  assert.equal(c.bands.length, 4, 'two series x two levels');
  assert.equal(c.bandKey, '80 / 95', 'levels are deduplicated');
  assert.deepStrictEqual(c.names, ['m', 'f', '80 / 95']);
});

test('colours are assigned over the visible series only', (t) => {
  const ids = forecastIds('m').concat(forecastIds('f'));
  const c = mount(ids, 24);
  t.after(() => c.close());

  // `f` is the sixth id. If the bounds consumed hues it would be PALETTE[5],
  // and the same data without intervals would draw it in a different colour.
  assert.equal(c.opts.series[1].stroke, PALETTE[0]);
  assert.equal(c.opts.series[6].stroke, PALETTE[1]);
});

test('a bound without its partner stays a line', (t) => {
  // What a `dplyr::filter()` downstream does. The remaining bound must be
  // visible, so the board can see that it happened.
  const c = mount(['m', 'm_lower_80', 'm_lower_95', 'm_upper_95'], 24, {
    m_lower_80: 20, m_lower_95: 20, m_upper_95: 20
  });
  t.after(() => c.close());

  assert.equal(c.bands.length, 1, 'only the 95 is a pair');
  assert.deepStrictEqual(c.names, ['m', 'm_lower_80', '95']);
  assert.equal(c.opts.series[2].width, 1.6, 'the orphan draws as a line');
});

test('a bound without its base stays a line', (t) => {
  // A band around a series that is not on the chart has nothing to be an
  // interval OF.
  const c = mount(['other', 'm_lower_80', 'm_upper_80']);
  t.after(() => c.close());

  assert.deepStrictEqual(c.bands, []);
  assert.deepStrictEqual(c.names, ['other', 'm_lower_80', 'm_upper_80']);
});

test('an id that merely looks like a bound is left alone', (t) => {
  // `sales_lower_80` is a perfectly good column name for something that is
  // not an interval. It only becomes one when its partner AND its base are
  // both on the chart, which is what keeps the convention from capturing
  // data it was never meant to.
  const c = mount(['sales_lower_80', 'sales_upper_80']);
  t.after(() => c.close());

  assert.deepStrictEqual(c.bands, [], 'no `sales` series to bracket');
  assert.equal(c.names.length, 2);
});

test('the split rule sits where the first bound starts', (t) => {
  const c = mount(forecastIds('m'), 24, {
    m_lower_80: 20, m_upper_80: 20, m_lower_95: 20, m_upper_95: 20
  });
  t.after(() => c.close());

  // Read from the DATA, not from a block argument: index 20 of the time
  // array, so a filter upstream moves the rule with it.
  assert.equal(c.splitX(), c.data[0][20]);
});

test('no bounds means no split rule', (t) => {
  const c = mount(['mdeaths', 'fdeaths']);
  t.after(() => c.close());
  assert.equal(c.splitX(), null);
});

test('a fractional level survives the round trip', (t) => {
  const c = mount(['m', 'm_lower_97.5', 'm_upper_97.5'], 24,
                  { 'm_lower_97.5': 20, 'm_upper_97.5': 20 });
  t.after(() => c.close());

  assert.equal(c.bands.length, 1);
  assert.equal(c.bandKey, '97.5');
});
