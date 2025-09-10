# blockr.ts

**Time Series Analysis Blocks for blockr**

Specialized time series functionality for blockr using tsbox and dygraphs. Provides blocks for loading, transforming, and visualizing time series data as interactive dygraphs.

## Installation

```r
# Install dependencies first
install.packages(c("tsbox", "dygraphs", "shiny"))

# Install blockr.core if not already installed
# remotes::install_github("blockr-org/blockr.core")

# Load the package
devtools::load_all()
```

## Quick Start

```r
library(blockr.core)
library(blockr.ts)

# Example 1: Dataset selector
blockr.core::serve(
  new_ts_dataset_block()
)

# Example 2: Transform Block
blockr.core::serve(
  new_ts_pc_block(),
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers))
)

```

## Available Blocks

This package provides three types of time series blocks:

### Data Blocks
- **`new_ts_airpassenger_block()`** - Classic AirPassengers dataset
- **`new_ts_dataset_block()`** - Access to all 25 built-in R time series datasets

### Transform Blocks
- **`new_ts_pc_block()`** - Compute percentage changes

### Display
All blocks automatically display results as interactive dygraphs with:
- Pan and zoom capabilities
- Range selector
- Hover tooltips with exact values

## Block Documentation

### `new_ts_airpassenger_block()`

Simple time series data block that displays the classic AirPassengers dataset.

**Parameters:**
- None (parameterless block)

**Example:**
```r
# Display AirPassengers data as an interactive dygraph
blockr.core::serve(
  new_ts_airpassenger_block()
)
```

### `new_ts_dataset_block()`

General time series data selector providing access to all 25 built-in R time series datasets.

**Parameters:**
- `dataset`: Name of the dataset to load (default: "AirPassengers")
  - 21 univariate series: AirPassengers, Nile, lynx, co2, etc.
  - 2 multivariate series: EuStockMarkets (4 series), Seatbelts (8 series)

**Features:**
- Dropdown selector with descriptive names
- Dynamic info panel showing dataset type and frequency
- Automatic conversion using `tsbox::ts_tbl()`
- Handles both univariate and multivariate series

**Example:**
```r
# Select and display any built-in time series
blockr.core::serve(
  new_ts_dataset_block(dataset = "EuStockMarkets")
)

# Chain with transformations
blockr.core::serve(
  new_ts_dataset_block(dataset = "co2"),
  new_ts_pc_block()
)
```

### `new_ts_pc_block()`

Simple transform block that computes percentage changes for time series data.

**Parameters:**
- None (applies `tsbox::ts_pc()` directly)

**Features:**
- No UI (simplest possible implementation)
- Calculates percentage change between consecutive observations
- First value will be NA (no previous value to compare)
- Works with both univariate and multivariate time series

**Example:**
```r
# Compute percentage changes in a pipeline
blockr.core::serve(
  new_ts_dataset_block(dataset = "AirPassengers"),
  new_ts_pc_block()
)

# Works with multivariate data
blockr.core::serve(
  new_ts_dataset_block(dataset = "EuStockMarkets"),
  new_ts_pc_block()
)

# Standalone usage with direct data input
blockr.core::serve(
  new_ts_pc_block(),
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers))
)
```

## Complete Workflow Examples

### Example 1: Analyzing Stock Market Changes
```r
# European stock market percentage changes
blockr.core::serve(
  new_ts_dataset_block(dataset = "EuStockMarkets"),
  new_ts_pc_block()
)
```

### Example 2: CO2 Concentration Trends
```r
# Mauna Loa CO2 concentration with percentage changes
blockr.core::serve(
  new_ts_dataset_block(dataset = "co2"),
  new_ts_pc_block()
)
```

### Example 3: Simple AirPassengers Display
```r
# Just display the classic dataset
blockr.core::serve(
  new_ts_airpassenger_block()
)
```

## Available Time Series Datasets

The `new_ts_dataset_block()` provides access to 25 built-in R time series:

**Univariate Series (21):**
- AirPassengers - Monthly airline passengers (1949-1960)
- co2 - Mauna Loa CO2 concentration
- Nile - River Nile flow (1871-1970)
- lynx - Canadian lynx trappings (1821-1934)
- sunspots - Monthly sunspot numbers (1749-1983)
- And 16 more...

**Multivariate Series (2):**
- EuStockMarkets - 4 European stock indices (DAX, SMI, CAC, FTSE)
- Seatbelts - 8 UK road casualty series (1969-1984)

## Development

### Testing
```r
# Load and test
devtools::load_all()
devtools::test()

# Test individual blocks
block <- new_ts_dataset_block(dataset = "lynx")
class(block)  # Should include "ts_block" for dygraph display
```

### Registration
```r
# Register blocks with blockr
register_ts_blocks()
```

---

**This package provides time series analysis capabilities for blockr.** Use it to load, transform, and visualize time series data with interactive dygraphs.