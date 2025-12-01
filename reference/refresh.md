# Refresh a Workspace or SA-Processing

Refresh a Workspace or SA-Processing

## Usage

``` r
jsap_refresh(
  jsap,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"),
  period = 0,
  start = NULL,
  end = NULL,
  info = c("All", "Data", "None")
)

jws_refresh(
  jws,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"),
  period = 0,
  start = NULL,
  end = NULL,
  info = c("All", "Data", "None")
)
```

## Arguments

- policy:

  refresh policy to apply (see details).

- period, start, end:

  to specify the span on which outliers will not be re-identified (i.e.:
  re-detected) when `policy = "Outliers"` or
  `policy = "Outliers_StochasticComponent"`. Span definition: `period`:
  numeric, number of observations in a year (12, 4...). `start` and
  `end`: first and last date from which outliers will not be
  re-identified, defined as arrays of two elements: year and first
  period (for example, if `period = 12`, `c(1980, 1)` for January 1980).
  If they are not specified, the outliers will be re-identified on the
  whole series.

- info:

  information to refresh.

- jws, jsap:

  Java Workspace or SA-Processing

## Details

Available refresh policies are:

**Current**: applying the current pre-adjustment reg-arima model and
adding the new raw data points as Additive Outliers (defined as new
intervention variables)

**Fixed**: applying the current pre-adjustment reg-arima model and
replacing forecasts by new raw data points.

**FixedParameters**: pre-adjustment reg-arima model is partially
modified: regression coefficients will be re-estimated but regression
variables, Arima orders and coefficients are unchanged.

**FixedAutoRegressiveParameters**: same as FixedParameters but Arima
Moving Average coefficients (MA) are also re-estimated, Auto-regressive
(AR) coefficients are kept fixed.

**FreeParameters**: all regression and Arima model coefficients are
re-estimated, regression variables and Arima orders are kept fixed.

**Outliers**: regression variables and Arima orders are kept fixed, but
outliers will be re-detected on the defined span, thus all regression
and Arima model coefficients are re-estimated

**Outliers_StochasticComponent**: same as "Outliers" but Arima model
orders (p,d,q)(P,D,Q) can also be re-identified.
