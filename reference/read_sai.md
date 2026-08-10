# Read an SA-item

`read_sai()` extracts all the information of a SA-item (see details).

## Usage

``` r
read_sai(jsai)
```

## Arguments

- jsai:

  Java SA-item object.

## Value

a list

## Details

A SA-item contains more information than just the results of an
estimation. Full information is extracted with the `read_sai()` function
that returns a list of 5 objects:

- `ts`: raw time series.

- `referenceSpec`: initial specification. Reference when refreshing and
  relaxing constraints.

- `estimationSpec`: specification used for the current estimation.

- `resultSpec`: specification containing all parameters stemming from
  `estimationSpec` (fully identified model).

- `results`: results of the estimation.

## Examples

``` r

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
# \donttest{
jws <- jws_open(file)

# Select SAProcessing
jsap1 <- jws_sap(jws, 1)

# Select SA-item (as java object)
jsai1 <- jsap_sai(jsap1, 3)

 # read SA-item
read_sai(jsai = jsai1)
#> $ts
#> $name
#> [1] "RF0899 (frozen)"
#> 
#> $moniker
#> $source
#> [1] ""
#> 
#> $id
#> [1] "ddc09f6c-9b45-4f9e-8c91-7d3ebf863a0c"
#> 
#> attr(,"class")
#> [1] "JD3_TSMONIKER"
#> 
#> $metadata
#> $metadata$`@timestamp`
#> [1] "2025-04-03"
#> 
#> $metadata$`@source`
#> [1] "Txt"
#> 
#> $metadata$`@id`
#> [1] "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&file=C%3A%5CUsers%5CYWYD5I%5CDocuments%5C00_RJD3_Developpement%5C00_Quick_Data_Tests%5CData%5CIPI_nace4.csv#seriesIndex=5"
#> 
#> 
#> $data
#>            Jan       Feb       Mar       Apr       May       Jun       Jul
#> 1990 111.98017 106.40722 119.29479 104.06842 111.40162 113.26593 137.68031
#> 1991 113.66007 102.14083 109.93337 114.10433 110.58483 116.18019 149.47288
#> 1992 117.67223 106.85055 119.12183 126.15968 105.85344 136.16431 140.57657
#> 1993 108.33656 102.18422 116.12746 102.92001  92.45556 108.30291 127.72081
#> 1994  94.54971  91.42880 106.16091  99.73296 102.20585 111.96834 124.56344
#> 1995 100.63432 105.50623 118.21418 101.54868 106.28329 121.44511 120.91249
#> 1996 128.39843 119.75416 132.54063 120.06373 122.28072 127.53932 142.82216
#> 1997 111.98399 114.99652 120.61000 123.71057 118.33480 125.00624 136.89582
#> 1998 122.09590 116.93875 125.33340 146.04064 116.84619 127.97769 143.99567
#> 1999 127.56552 121.51214 132.55899 124.56279 115.58122 141.70717 138.17692
#> 2000 117.33993 117.86535 131.70287 116.19777 133.02464 124.97383 128.51653
#> 2001 136.58114 127.47884 136.39675 122.82334 129.16321 135.74880 138.26856
#> 2002 120.62738 106.76884 112.05426 109.38146 116.81737 102.43594 122.20700
#> 2003 127.03074 119.14995 131.98683 121.68155 117.71026 123.73558 133.70584
#> 2004 125.51551 111.23217 123.73557 118.42736 107.01738 131.40606 128.55369
#> 2005 126.08225 119.81587 134.43447 137.88734 122.07844 129.54938 116.77939
#> 2006 116.81535 107.39538 128.00768 110.61096 119.76047 128.23184 117.12287
#> 2007 119.80508 105.93946 123.67290 108.94290 110.85610 124.83011 123.22781
#> 2008 104.76819 130.26636 112.21151 134.05148 121.49767 126.58544 128.99294
#> 2009  95.86592  96.84103  83.93245  73.42534  62.17197  90.50661 103.31763
#> 2010  92.72872  91.63029 113.15865 101.92935  96.23019 110.99885 108.78505
#> 2011  98.22572 103.76875 111.68081  98.45033 114.39733 109.49087 111.77548
#> 2012 101.77256  89.49489  91.16292 101.76190 105.04459 105.55559 114.51839
#> 2013  97.60549 105.12530 105.96804  98.59948  88.96558  97.87360 106.14588
#> 2014 108.32803 104.36544 109.09925 105.73119 102.29739 101.41161 119.78952
#> 2015  94.24561  98.94991 112.70495  88.17863  89.45407  97.22399  94.85769
#> 2016 103.81344  99.79168  87.16486 113.09241 114.52146 118.13172 112.31991
#> 2017 112.81103 121.09764 134.97269  95.63631 119.43010  65.48361  96.53360
#> 2018 108.60152 106.78437 116.87420 108.10013 115.64081  98.39516 120.83236
#> 2019 101.60982  96.41422 104.88769  95.18881 101.19053 105.33432 102.87528
#> 2020  97.31089  95.26518  85.76747  74.17675  56.55460  59.71036  92.64317
#> 2021 104.85271 105.78811 116.71424 114.66855 101.34767 105.14274  89.72527
#> 2022  91.86406 102.90153 113.43710 101.63429  98.42886  90.89587  93.27999
#> 2023  88.59938  93.96608 101.74261  84.75229  86.35975  94.94654  88.88975
#> 2024  88.21640  88.85806  96.89828  99.10996  95.52958  91.57847          
#>            Aug       Sep       Oct       Nov       Dec
#> 1990  52.55658  81.87243 127.59827 121.10457  97.01415
#> 1991  46.07469 114.77328 126.80340 115.12363 111.43670
#> 1992  35.60427 119.10939 117.14055 107.06231  98.61792
#> 1993  31.28009 112.43083 100.44586  99.15789  91.86783
#> 1994  39.55558 108.41405 105.23316  97.22535 104.98378
#> 1995  69.14657 116.58196 113.69861 108.77993  87.25713
#> 1996  74.29661 124.12149 131.62377 108.83783  90.24022
#> 1997  71.55120 115.63636 136.61778 109.01689  95.53962
#> 1998  75.27041 134.49850 137.16171 120.40358 109.80494
#> 1999  78.50741 130.59087 123.28671 118.59456 115.67418
#> 2000  84.83709 134.74701 137.46197 128.33917 110.94990
#> 2001  89.51941 125.06668 121.74332 116.62332  85.09859
#> 2002  69.45571 109.78621 121.75284 103.34083  88.93241
#> 2003  82.65245 126.68890 134.95910 115.47870 103.30540
#> 2004  85.55618 134.36555 121.45620 125.62430 102.41849
#> 2005  91.51707 130.73200 119.71370 118.43659 108.02745
#> 2006  83.41014 118.05962 114.51375 110.11624 100.03320
#> 2007  93.71000 106.39368 127.31772 115.44529  99.33299
#> 2008  81.23834 113.18715 108.71695  85.77463  66.14663
#> 2009  66.03259 110.35779 101.35054  93.21770  71.51158
#> 2010  89.10375 112.31140 109.50830 112.37865  90.54569
#> 2011  95.05654 113.99807  97.76763  97.94501  77.42709
#> 2012  76.82779 107.76669 131.90395 123.59141  81.81006
#> 2013  74.41880  97.31120 117.44245  97.65566  79.82971
#> 2014  78.19819 109.65746 113.51701  92.99477  74.99735
#> 2015  65.06826 103.12478 109.37535 103.90976  82.62499
#> 2016  84.37125 118.20611 116.69092 118.76118 101.17812
#> 2017  86.19279 118.78479 120.00556 120.92885  92.18021
#> 2018  85.71753  98.56561 111.11869 101.45262  70.43689
#> 2019  80.20773  99.27261 108.54179  93.97769  74.77705
#> 2020  61.55135  99.18094 111.83976  98.15058  98.95871
#> 2021  74.30368 104.32938 100.33694  78.76491 104.02580
#> 2022  82.29402  98.72387  83.43379  92.50002  64.37818
#> 2023  74.50032  93.86312  97.33622  81.72218  61.72457
#> 2024                                                  
#> 
#> attr(,"class")
#> [1] "JD3_TS"
#> 
#> $referenceSpec
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
#> Mode X-11: UNKNOWN
#> Bias: RATIO
#> 
#> Benchmarking
#> Is enabled: No
#> 
#> $estimationSpec
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
#> Mode X-11: UNKNOWN
#> Bias: RATIO
#> 
#> Benchmarking
#> Is enabled: No
#> 
#> $resultSpec
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
#>  - Value: -4.566495 
#> 
#> Pre-specified outliers: 6
#>  - AO (1990-09-01), coefficient: -30.2223704666036 (ESTIMATED)
#>  - TC (2009-03-01), coefficient: -28.1358496347564 (ESTIMATED)
#>  - AO (2016-03-01), coefficient: -31.1369678944946 (ESTIMATED)
#>  - AO (2017-06-01), coefficient: -49.5183798446616 (ESTIMATED)
#>  - TC (2020-03-01), coefficient: -28.3006144995563 (ESTIMATED)
#>  - TC (2020-05-01), coefficient: -31.8747397192961 (ESTIMATED)
#> Ramps: No
#> 
#> Outliers
#> Is enabled: No
#> 
#> ARIMA
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>   -0.6253   -0.7067 
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
#> Mode X-11: UNKNOWN
#> Bias: RATIO
#> 
#> Benchmarking
#> Is enabled: No
#> 
#> $results
#> NULL
#> 
# }

```
