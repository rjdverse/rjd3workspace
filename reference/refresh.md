# Refresh a Workspace or SA-Processing

Refresh a Workspace or SA-Processing

## Usage

``` r
jsap_refresh(
  jsap,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"),
  period = 0,
  start = NULL,
  end = NULL,
  info = c("All", "Data", "None")
)

jws_refresh(
  jws,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"),
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

  additional parameters used to specify the span When
  `policy = "Outliers"` or `policy = "Outliers_StochasticComponent"`
  `period`: numeric, number of observations in a year (12, 4...),
  compulsory, if mis-specified or missing, re-estimation with refreshed
  specification won't work. `end` has to be specified as the date from
  which outliers will be re-identified

- info:

  indication on how data should be refreshed `All`: data and metadata
  will be refreshed (default) `Data`: data will be refreshed, not
  metadata `None`: nor data neither metadata will be refreshed, to be
  used for updating specifications only.

- jws, jsap:

  Java Workspace or SA-Processing

## Value

refreshed workspace or SAP

## Details

A particular selection of parameters to be kept fixed or re-estimated is
called a revision policy. Workspace has to be computed before refresh
When refreshing data, empty your cache by restarting your R session,
before refreshing, otherwise the specification will be refreshed but the
new data will not be taken into account.

Available refresh policies are:

1.  **Fixed**: applying the current pre-adjustment reg-arima model and
    replacing forecasts by new raw data points; X11 (or SEATS) and
    Benchmarking part parameters are untouched.

2.  **FixedParameters**: pre-adjustment reg-arima model is partially
    modified: regression coefficients will be re-estimated but
    regression variables, Arima orders and coefficients are unchanged;
    X11 (or SEATS) and Benchmarking part parameters are untouched.

3.  **FixedAutoRegressiveParameters**: same as FixedParameters but Arima
    Moving Average coefficients (MA) are also re-estimated,
    Auto-regressive (AR) coefficients are kept fixed; X11 (or SEATS) and
    Benchmarking part parameters are untouched.

4.  **FreeParameters**: all regression and Arima model coefficients are
    re-estimated, regression variables and Arima orders are kept fixed;
    X11 (or SEATS) and Benchmarking part parameters are untouched.

5.  **Outliers**: regression variables and Arima orders are kept fixed,
    but outliers will be re-detected on the defined span, thus all
    regression and Arima model coefficients are re-estimated; X11 (or
    SEATS) and Benchmarking part parameters are untouched.

6.  **Outliers_StochasticComponent**: same as "Outliers" but Arima model
    orders (p,d,q)(P,D,Q) can also be re-identified; X11 (or SEATS) and
    Benchmarking part parameters are untouched.

7.  **Complete**: All the parameters are re-identified and re-estimated,
    unless constrained in the reference spec. X11 (or SEATS) and
    Benchmarking part parameters are entirely reset to values in the
    reference specification.

## References

More information on revision policies in JDemetra+ documentation:
<https://doc.jdemetra.org/a-rev-policies>

## Examples

``` r

# Load workspace
file <- system.file("workspaces", "workspace_test_refresh.xml", package = "rjd3workspace")
# \donttest{
jws <- jws_open(file)
jws_compute(jws)
# Read current workspace: reference spec and estimation spec
rws <- read_workspace(jws, compute= TRUE)
rws$processing$`SAProcessing-1`$`RF0811`$referenceSpec
#> Specification
#> 
#> Series
#> Serie span: All 
#> Preliminary Check: Yes
#> 
#> Estimate
#> Model span: All 
#> 
#> Tolerance: 1e-07
#> 
#> Transformation
#> Function: AUTO
#> AIC difference: -2
#> Adjust: NONE
#> 
#> Regression
#> No calendar regressor
#> 
#> Easter: No
#> 
#> Pre-specified outliers: 1
#>  - LS.2024-01, coefficient: 0 (UNDEFINED)
#> Ramps: No
#> 
#> Outliers
#> Detection span: All 
#> Outliers type: 
#>  - AO, critical value : 0 (Auto)
#>  - LS, critical value : 0 (Auto)
#>  - TC, critical value : 0 (Auto)
#> TC rate: 0.7 (Auto)
#> Method: ADDONE (Auto)
#> 
#> ARIMA
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>         0         0 
#> 
#> Specification X11
#> Seasonal component: Yes
#> Length of the Henderson filter: 0
#> Seasonal filter: FILTER_MSR
#> Boundaries used for extreme values correction : 
#>   lower_sigma:  1.5 
#>   upper_sigma:  2.5
#> Nb of forecasts: -1
#> Nb of backcasts: 0
#> Calendar sigma: NONE
#> 
#> Benchmarking
#> Is enabled: No
rws$processing$`SAProcessing-1`$`RF0811`$estimationSpec
#> Specification
#> 
#> Series
#> Serie span: From 2012-01-01 
#> Preliminary Check: Yes
#> 
#> Estimate
#> Model span: From 2015-01-01 
#> 
#> Tolerance: 1e-07
#> 
#> Transformation
#> Function: LEVEL
#> AIC difference: -2
#> Adjust: NONE
#> 
#> Regression
#> No calendar regressor
#> 
#> Easter: JULIAN 
#> Duration: 8 (Auto) 
#> Test: NO  
#> Coef:
#>  - Type: UNDEFINED  
#>  - Value: 0 
#> 
#> Pre-specified outliers: 4
#>  - AO.2020-03, coefficient: 0 (UNDEFINED)
#>  - AO.2020-04, coefficient: 0 (UNDEFINED)
#>  - AO.2020-05, coefficient: 0 (UNDEFINED)
#>  - AO.2024-04, coefficient: 0 (UNDEFINED)
#> Ramps: No
#> 
#> Outliers
#> Detection span: All 
#> Outliers type: 
#>  - AO, critical value : 0 (Auto)
#>  - LS, critical value : 0 (Auto)
#> TC rate: 0.7 (Auto)
#> Method: ADDONE (Auto)
#> 
#> ARIMA
#> SARIMA model: (1,1,1) (1,1,1)
#> 
#> SARIMA coefficients:
#>    phi(1)  theta(1)   bphi(1) btheta(1) 
#>         0         0         0         0 
#> 
#> Specification X11
#> Seasonal component: Yes
#> Length of the Henderson filter: 0
#> Seasonal filter: FILTER_MSR
#> Boundaries used for extreme values correction : 
#>   lower_sigma:  1.5 
#>   upper_sigma:  2.5
#> Nb of forecasts: -2
#> Nb of backcasts: 0
#> Calendar sigma: NONE
#> 
#> Benchmarking
#> Enabled: Yes
#> Target: TARGET_CALENDARADJUSTED (Auto)
#> Lambda: 1 (Auto)
#> Rho: 1 (Auto)
#> Bias: BIAS_NONE (Auto)
#> Use forecast: Yes
# Refresh workspace COMPLETE
jws_refresh(jws, policy = "Complete")
# Read refreshed workspace: new estimation spec
rws2 <- read_workspace(jws, compute= TRUE)
rws2$processing$`SAProcessing-1`$`RF0811`$estimationSpec
#> Specification
#> 
#> Series
#> Serie span: All 
#> Preliminary Check: Yes
#> 
#> Estimate
#> Model span: All 
#> 
#> Tolerance: 1e-07
#> 
#> Transformation
#> Function: AUTO
#> AIC difference: -2
#> Adjust: NONE
#> 
#> Regression
#> No calendar regressor
#> 
#> Easter: No
#> 
#> Pre-specified outliers: 1
#>  - LS.2024-01, coefficient: 0 (UNDEFINED)
#> Ramps: No
#> 
#> Outliers
#> Detection span: All 
#> Outliers type: 
#>  - AO, critical value : 0 (Auto)
#>  - LS, critical value : 0 (Auto)
#>  - TC, critical value : 0 (Auto)
#> TC rate: 0.7 (Auto)
#> Method: ADDONE (Auto)
#> 
#> ARIMA
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>         0         0 
#> 
#> Specification X11
#> Seasonal component: Yes
#> Length of the Henderson filter: 0
#> Seasonal filter: FILTER_MSR
#> Boundaries used for extreme values correction : 
#>   lower_sigma:  1.5 
#>   upper_sigma:  2.5
#> Nb of forecasts: -1
#> Nb of backcasts: 0
#> Calendar sigma: NONE
#> 
#> Benchmarking
#> Is enabled: No
# Refresh workspace Outliers (like "lastoutliers in the GUI, but with custom start date)
jws <- jws_open(file)
jws_compute(jws)
jws_refresh(jws, policy = "Outliers", period=12, end=c(2020,4))
# Read refreshed workspace: new estimation spec
rws3 <- read_workspace(jws, compute= TRUE)
rws3$processing$`SAProcessing-1`$`RF0811`$estimationSpec
#> Specification
#> 
#> Series
#> Serie span: From 2012-01-01 
#> Preliminary Check: Yes
#> 
#> Estimate
#> Model span: From 2015-01-01 
#> 
#> Tolerance: 1e-07
#> 
#> Transformation
#> Function: LEVEL
#> AIC difference: -2
#> Adjust: NONE
#> 
#> Regression
#> No calendar regressor
#> 
#> Easter: STANDARD 
#> Duration: 8 (Auto) 
#> Test: NO  
#> Coef:
#>  - Type: ESTIMATED  
#>  - Value: -18.74348 
#> 
#> Pre-specified outliers: 2
#>  - LS.2024-01, coefficient: 0 (INITIAL)
#>  - AO.2020-03, coefficient: -23.2364877397156 (INITIAL)
#> Ramps: No
#> 
#> Outliers
#> Detection span: From 2020-04-01 
#> Outliers type: 
#>  - AO, critical value : 0 (Auto)
#>  - LS, critical value : 0 (Auto)
#>  - TC, critical value : 0 (Auto)
#> TC rate: 0.7 (Auto)
#> Method: ADDONE (Auto)
#> 
#> ARIMA
#> SARIMA model: (1,1,1) (1,1,1)
#> 
#> SARIMA coefficients:
#>    phi(1)  theta(1)   bphi(1) btheta(1) 
#>         0         0         0         0 
#> 
#> Specification X11
#> Seasonal component: Yes
#> Length of the Henderson filter: 0
#> Seasonal filter: FILTER_MSR
#> Boundaries used for extreme values correction : 
#>   lower_sigma:  1.5 
#>   upper_sigma:  2.5
#> Nb of forecasts: -2
#> Nb of backcasts: 0
#> Calendar sigma: NONE
#> 
#> Benchmarking
#> Enabled: Yes
#> Target: TARGET_CALENDARADJUSTED (Auto)
#> Lambda: 1 (Auto)
#> Rho: 1 (Auto)
#> Bias: BIAS_NONE (Auto)
#> Use forecast: Yes
# }
```
