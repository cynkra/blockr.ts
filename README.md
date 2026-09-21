# blockr.ts

Time series support for [blockr](https://blockr-org.github.io/blockr.site/).
Two things: a convention about what a time series *is*, and a preview that
draws one. Six blocks build on top of them.

## The convention

**A time series is any data frame [tsbox](https://docs.ropensci.org/tsbox/)
can read as one.** No class, no wrapper, no declaration step. A tsboxable
frame is a data frame, so every dplyr, io and viz block keeps working on it,
and anything those blocks produce is still a time series if it still looks
like one.

tsbox decides, and its refusals do the work. A table of orders with a date and
an amount *is* a time series and draws as one. A table with one row per order
and several orders a day is not: `ts_tbl()` errors with "duplicated
information" and the frame falls through to the ordinary table preview. So do
`mtcars`, `iris` and anything with two numeric columns, where tsbox declines
rather than guessing which one is the value.

Where tsbox cannot read a frame, the fix is **blockr.dplyr**, not a block of
ours. A CSV of `month` (as the text `"2021-01"`), `sales`, `costs` needs a
select to drop the second numeric column and a rename to `time` and `value`,
which is also what makes tsbox parse the month strings into dates.

## The preview

`ts_display` is a `blockr.core::tabular_display`. Turn it on per app:

```r
options(blockr.tabular_display = blockr.ts::ts_display)
```

It draws when it can resolve the result and hands **everything else to the
display the app already had**, so switching it on cannot change how a
non-series block looks. Every drawable block gets a chart/table switch at the
top right of its preview; the table side is the fallback display rendering it.

The chart is [uPlot](https://github.com/leeoniya/uPlot) 1.6.32, vendored in
`inst/js`. Measured on `EuStockMarkets`, 1860 × 4 points: 22 KB gzipped and
1.8 ms to draw, against 363 KB and 8.7 ms for ECharts on the same payload.

`ts_resolve()` is the seam for results tsbox cannot read. A package whose
block produces a model adds a method rather than a branch inside the chart,
which is how a fitted forecast draws without the chart knowing what one is.

## The blocks

| Block | Takes | Gives |
|---|---|---|
| `new_ts_dataset_block()` | — | a built-in `ts` or `mts` object as a long frame |
| `new_ts_transform_block()` | one series | the same series, transformed |
| `new_ts_combine_block()` | two or more | one frame |
| `new_ts_pick_block()` | many series | a few |
| `new_ts_forecast_block()` | a series | **fitted models** |
| `new_ts_forecast_series_block()` | fitted models | a frame |

### Transform

One block for all sixteen unary tsbox verbs: changes, index, span, lag,
frequency, trend, gaps. Pick the function from the tile grid in the gear; the
arguments under it are the ones that function takes, and they are built from
`tsbox::ts_summary()` of whatever arrived. The base-period list is bounded by
the series, the frequency list offers only frequencies coarser than the data
has, and "one year" in the lag list is 12 or 4 because the data said so.

```r
new_ts_transform_block(verb = "pcy")
new_ts_transform_block(verb = "index", base = "1954-01-01")
```

Sixteen constructors would be sixteen copies of one file, and changing your
mind about a verb would be a delete and two reconnections rather than a click.

### Pick

A list with one row per series, a sparkline of its shape beside each. One
Census MARTS download is 168 series whose names differ by a couple of words,
so a multi-select of 168 ids gives you nothing to choose on; the shape does.

### Forecast

**The block returns the model, not a frame.** A forecast is a point path, two
or more interval paths, the fitted values, the residuals and whatever the
engine chose, and collapsing that at the block boundary throws away everything
the second question needs. The preview draws the model; the hamburger switches
to the model output, which is the fit and its parameters at one target and a
table sorted worst-first at several.

```r
new_ts_forecast_block(engine = "arima", h = 24)
new_ts_forecast_block(target = c("mdeaths", "fdeaths"), engine = "var")
```

Engines are `"ets"`, `"arima"`, `"theta"` and `"var"`. The target is always a
multiselect and the result is always a named list of models, length one when
one series is picked: a control that changes the output type when you touch it
breaks the board downstream. For every engine but VAR, several targets means a
loop; VAR treats the set as one system.

`new_ts_forecast_series_block()` turns the models back into a frame: the
forecast, the fitted values, the residuals, the components, or one row of
accuracy per series.

**The interval convention lives on the extractor**, because `(id, time, value)`
has no column for an interval and the only representation that stays tsboxable
is more series:

```
mdeaths
mdeaths_lower_80
mdeaths_upper_80
```

The chart pairs `_lower_`/`_upper_` back into a band. The convention being in
the id is what makes it survive the trip: a frame can go through three dplyr
blocks and still draw its band, and a board that never heard of forecasting
draws five lines rather than failing. An attribute on the frame survives
nothing.

## Install

```r
pak::pak("blockr-org/blockr.ts")
```

`vars` (VAR) and `broom` (the model output) are Suggests.

## Try it

```r
pkgload::load_all("blockr.ts")
options(blockr.tabular_display = blockr.ts::ts_display)

blockr.core::serve(
  blockr.core::new_static_block(
    tsbox::ts_tbl(tsbox::ts_c(mdeaths = datasets::mdeaths,
                              fdeaths = datasets::fdeaths))
  )
)
```

`dev/` has three boards: `app-ts-blocks.R` for the four data blocks,
`app-ts-forecast.R` for the two forecast blocks, `app-ts-display.R` for the
preview on its own.
