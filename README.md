# blockr.ts

**Time Series Analysis Blocks for blockr**

Specialized time series functionality for blockr using the tsbox package. Provides blocks for loading, transforming, and visualizing time series data with seamless format conversion.

## Installation

```r
# Install dependencies first
install.packages(c("blockr.core", "tsbox", "htmlwidgets", "ggplot2", "plotly"))

# Load the package
devtools::load_all()
```

## Quick Start

```r
library(blockr.core)
library(blockr.ts)

# Complete time series workflow
serve(
  new_ts_block(dataset = "AirPassengers"),         # Load time series data
  new_ts_pc_block(method = "pcy"),                 # Calculate year-over-year % change
  new_ts_plot_block(title = "Air Travel Growth")   # Create interactive visualization
)
```

This creates a complete workflow: **Load Data** → **Calculate % Change** → **Interactive Plot**

## Time Series Features

This package provides specialized time series functionality for blockr using tsbox for format conversion and analysis.

### Key Features

✅ **Time series data blocks** with multiple format support
✅ **Percentage change calculations** with multiple methods (pc, pcy, pca, diff, diffy)
✅ **tsbox integration**: Convert between tibble, ts, xts, zoo formats
✅ **Built-in datasets**: economics, AirPassengers, lynx
✅ **Time filtering**: Optional start year filtering
✅ **Transform blocks**: Chain data loading with percentage change calculations
✅ **Clean UI layout** with time series specific controls
✅ **Screenshot validation** infrastructure included

### Time Series Data Sources

1. **Built-in datasets**:
   - `economics`: US economic time series data (ggplot2)
   - `AirPassengers`: Classic airline passenger data
   - `lynx`: Annual lynx trappings in Canada

2. **Format conversion**:
   - Tibble: Standard data.frame format
   - TS: Classic R time series format
   - XTS: Extensible time series
   - Zoo: S3 infrastructure for irregular time series

3. **Time filtering**:
   - Optional start year parameter
   - Automatic date/time column detection
   - Preserves time series structure

4. **Test and validate**:
   ```r
   # Generate screenshots to verify functionality
   source("inst/scripts/generate_ts_screenshot.R")

   # Test the block works
   devtools::load_all()
   blockr.core::serve(new_ts_block())
   ```

## Block Documentation

### `new_ts_block()`

Creates a time series data block with format conversion capabilities.

**Parameters:**
- `dataset`: Time series dataset - "economics", "AirPassengers", or "lynx" (default: "economics")
- `start_year`: Optional starting year to filter data (default: NULL)
- `convert_format`: Output format - "tibble", "ts", "xts", or "zoo" (default: "tibble")

**Example configurations:**

```r
# Basic usage
basic_block <- new_ts_block()

# Air passengers data as time series
air_block <- new_ts_block(dataset = "AirPassengers", convert_format = "ts")

# Economics data from 2000 onwards as XTS
recent_block <- new_ts_block(
  dataset = "economics",
  start_year = 2000,
  convert_format = "xts"
)
```

### `new_ts_pc_block()` - Percentage Change Transformations

Creates a time series percentage change transform block using tsbox functions.

**Parameters:**
- `method`: Calculation method with 5 options (default: "pc"):
  - `"pc"`: Period-to-period percentage change
  - `"pcy"`: Year-over-year percentage change
  - `"pca"`: Annualized percentage change rates
  - `"diff"`: First differences (absolute changes)
  - `"diffy"`: Year-over-year differences

**Block Features:**
- ✅ **Input validation**: Accepts any tsboxable object
- ✅ **Multiple methods**: 5 different calculation types
- ✅ **Consistent output**: Always returns `ts_tbl()` format
- ✅ **Seamless chaining**: Works with any time series input

```r
# Examples of different calculation methods
basic_pc_block <- new_ts_pc_block()                    # Period-to-period %
yoy_block <- new_ts_pc_block(method = "pcy")           # Year-over-year %
annual_block <- new_ts_pc_block(method = "pca")        # Annualized %
diff_block <- new_ts_pc_block(method = "diff")         # First differences
yoy_diff_block <- new_ts_pc_block(method = "diffy")    # YoY differences
```

### `new_ts_plot_block()` - Interactive Visualizations

Creates interactive time series plots using tsbox and plotly.

**Parameters:**
- `title`: Plot title (default: "Time Series Plot")
- `color_by`: Color by series for multiple time series (default: TRUE)
- `theme`: ggplot theme - "minimal", "classic", "bw", "grey" (default: "minimal")
- `line_size`: Line thickness (default: 1)

**Block Features:**
- ✅ **Interactive plots**: Uses `ts_ggplot()` + `plotly::ggplotly()`
- ✅ **Multiple themes**: 4 professional ggplot2 themes
- ✅ **Flexible input**: Accepts tsboxable objects or ts_tbl output
- ✅ **Responsive UI**: Bootstrap layout with intuitive controls

```r
# Examples of different plot configurations
basic_plot <- new_ts_plot_block()
custom_plot <- new_ts_plot_block(
  title = "Economic Analysis",
  theme = "classic",
  line_size = 1.5
)
```

**Complete Workflow Example:**

```r
# Three-block workflow: Data → Transform → Visualize
library(blockr.core)
library(blockr.ts)

serve(
  new_ts_block(dataset = "AirPassengers"),
  new_ts_pc_block(method = "pcy"),                 # Year-over-year growth
  new_ts_plot_block(
    title = "Air Passengers: Annual Growth Rate",
    theme = "minimal"
  )
)
```

## Development Features

### Screenshot Validation

The package includes proven screenshot validation infrastructure:

```r
# Generate screenshots to verify blocks work
source("inst/scripts/generate_ts_screenshot.R")
```

Screenshots demonstrate that blocks are working correctly by showing both:
- ✅ **UI controls** (parameter inputs)
- ✅ **Data output** (rendered data table)

### Essential blockr Patterns Demonstrated

1. **Reactive Values**: `r_` prefix pattern (`r_dataset`, `r_start_year`, `r_convert_format`)
2. **Expression Building**: Modern `parse(text = glue::glue())` pattern with conditional logic
3. **State Management**: All constructor parameters included in state list
4. **Clean UI**: Time series specific grid layout with sections
5. **Internal Helpers**: `get_ts_data()` shows tsbox integration patterns

### Documentation

- **CLAUDE.md**: Comprehensive development documentation with blockr patterns
- **README.md**: User-facing documentation and template usage instructions
- **Example scripts**: `inst/examples/simple_example.R` for testing

## Why Use This Template?

### Compared to Creating from Scratch
- ✅ All essential patterns already implemented
- ✅ Screenshot validation infrastructure ready
- ✅ Documentation templates included
- ✅ Known working structure

### Compared to Complex Packages
- ✅ Minimal complexity (3 params vs 13+)
- ✅ No external dependencies to debug
- ✅ Clear, understandable code
- ✅ Easy to modify and extend

## Success Validation

A working blockr.ts package should:

1. **Generate working screenshots** showing UI + time series data
2. **Load without errors**: `devtools::load_all()`
3. **Register blocks properly**: Block appears in blockr registry
4. **Serve successfully**: `blockr.core::serve(new_ts_block())`
5. **Show time series output**: Screenshots show both controls and formatted time series data

The screenshot above confirms all these criteria are met ✅

---

**This package provides time series analysis capabilities for blockr.** Use it to load, filter, and convert time series data in your blockr workflows.