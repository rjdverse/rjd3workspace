# Get Specification in a Sa-Item

`get_estimation_specification()` extract the estimation specification,
`get_domain_specification()` the domain specification, ,
`get_active_specification()` the active specification
`get_point_specification()` the point specification

## Usage

``` r
get_domain_specification(jsai)

get_estimation_specification(jsai)

get_point_specification(jsai)

get_active_specification(jsai)
```

## Arguments

- jsai:

  Java SA-item object.

## Value

the specification

## Examples

``` r
# Load a Workspace to modify
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
# \donttest{
jws <- jws_open(file)

# Select SAProcessing with the target SA-item
jsap1 <- jws_sap(jws, 1)
jsai1 <- jsap_sai(jsap1, 1)

# Get the active specification in targeted SA-item
get_active_specification(jsai1)
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
#> Calendar regressor: TradingDays
#> with Leap Year: Yes
#> AutoAdjust: TRUE
#> Test: REMOVE
#> 
#> Easter: STANDARD 
#> Duration: 8 (Auto) 
#> Test: ADD (Auto) 
#> 
#> Pre-specified outliers: 0
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

# Get the domain specification in targeted SA-item
get_domain_specification(jsai1)
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
#> Calendar regressor: TradingDays
#> with Leap Year: Yes
#> AutoAdjust: TRUE
#> Test: REMOVE
#> 
#> Easter: STANDARD 
#> Duration: 8 (Auto) 
#> Test: ADD (Auto) 
#> 
#> Pre-specified outliers: 0
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

# Get the estimation specification in targeted SA-item
get_estimation_specification(jsai1)
#> NULL

# Get the point specification in targeted SA-item
get_point_specification(jsai1)
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
#> Function: LEVEL
#> AIC difference: -2
#> Adjust: NONE
#> 
#> Regression
#> Calendar regressor: TradingDays
#> with Leap Year: Yes
#> AutoAdjust: FALSE
#> Test: NO
#> 
#> Easter: STANDARD 
#> Duration: 1  
#> Test: NO  
#> Coef:
#>  - Type: ESTIMATED  
#>  - Value: -4.146917 
#> 
#> Pre-specified outliers: 6
#>  - AO (1991-02-01), coefficient: -22.8943163122879 (ESTIMATED)
#>  - AO (1997-01-01), coefficient: -19.7051044587958 (ESTIMATED)
#>  - AO (2011-05-01), coefficient: 21.9820917691723 (ESTIMATED)
#>  - AO (2012-02-01), coefficient: -26.8246349465165 (ESTIMATED)
#>  - TC (2020-03-01), coefficient: -43.6342415282156 (ESTIMATED)
#>  - AO (2020-04-01), coefficient: -34.5210526944179 (ESTIMATED)
#> Ramps: No
#> 
#> Outliers
#> Is enabled: No
#> 
#> ARIMA
#> SARIMA model: (2,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>    phi(1)    phi(2)  theta(1) btheta(1) 
#>  -0.03487   0.14440  -0.60687  -0.82910 
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
# }
```
