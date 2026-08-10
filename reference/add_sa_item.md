# Add a SA-item to a SAProcessing

Add a SA-item to a SAProcessing

## Usage

``` r
add_sa_item(jsap, name, x, spec)

# S3 method for class 'ts'
add_sa_item(jsap, name, x, spec)

# Default S3 method
add_sa_item(jsap, name, x, spec)

# S3 method for class 'jobjRef'
add_sa_item(jsap, name, x, spec)
```

## Arguments

- jsap:

  SAProcessing.

- name:

  name of the SA-item to be added.

- x:

  either a seasonal adjustment model (from
  [`rjd3x13::x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.html)
  or
  [`rjd3tramoseats::tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.html)),
  a SA-item object, `"ts"` object.

- spec:

  specification to use when `x` is a `"ts"` object.

## Value

`NULL` returned invisibly#'

## Examples

``` r

dir <- tempdir()

# Raw series
y <- rjd3toolkit::ABS$X0.2.09.10.M

# Creating an empty workspace and SAProcessing
jws <- jws_new()
jsap1 <- jws_sap_new(jws, "sap1")

# Adding SA-item as estimation result

# \donttest{
# Estimation with rjd3x13
add_sa_item(jsap1, name = "series_1", x = rjd3x13::x13(y))

# Estimation with rjd3tramoseats
add_sa_item(jsap1, name = "series_2", x = rjd3tramoseats::tramoseats(y))

# Adding SA-item as raw series + specification
add_sa_item(jsap1, name = "series_3", x = y, rjd3x13::x13_spec("RSA3"))
add_sa_item(jsap1, name = "series_4", x = y, rjd3tramoseats::tramoseats_spec("RSAFull"))

jsai1 <- jsap_sai(jsap = jsap1, idx = 1L)
# Adding SA-item from a Workspace
add_sa_item(jsap = jsap1, name = "series_1_bis", x = jsai1)

rws <- read_workspace(jws)
rws$processing$sap1$series_4
#> $ts
#> $name
#> [1] "series_4"
#> 
#> $moniker
#> $source
#> [1] ""
#> 
#> $id
#> [1] "97d9ecf3-3bc4-4965-bf47-ef42319590a6"
#> 
#> attr(,"class")
#> [1] "JD3_TSMONIKER"
#> 
#> $metadata
#> NULL
#> 
#> $data
#>         Jan    Feb    Mar    Apr    May    Jun    Jul    Aug    Sep    Oct
#> 1982                       460.1  502.6  443.8  459.1  438.4  465.1  452.7
#> 1983  379.2  378.0  472.1  503.4  510.6  462.4  468.3  458.2  482.7  485.3
#> 1984  414.7  414.5  484.7  487.3  597.9  500.4  543.4  503.4  522.8  556.6
#> 1985  516.3  452.5  525.8  587.7  700.3  561.8  602.8  582.5  563.1  637.1
#> 1986  570.5  478.2  547.4  594.3  751.6  553.4  663.2  581.1  661.9  665.6
#> 1987  613.9  513.2  599.9  674.1  714.0  670.5  720.9  601.6  672.3  709.1
#> 1988  631.0  551.1  678.1  715.7  740.8  722.0  683.5  650.9  723.3  729.6
#> 1989  631.5  552.0  719.0  697.6  764.8  786.3  715.1  723.8  757.9  751.7
#> 1990  678.2  586.2  726.8  744.1  815.5  832.4  710.3  759.4  741.1  786.6
#> 1991  694.0  604.7  719.2  748.2  828.2  746.9  794.5  770.4  741.5  858.6
#> 1992  740.0  665.9  701.5  831.4  878.6  826.0  788.2  723.6  819.8  902.5
#> 1993  762.1  643.0  754.1  840.7  906.6  887.1  771.5  728.7  844.7  886.9
#> 1994  745.7  664.4  821.5  831.7  908.0  912.6  782.9  798.8  887.0  934.6
#> 1995  752.4  682.5  811.2  906.0  927.2  906.8  880.6  873.9  856.8  920.6
#> 1996  833.1  737.1  812.0  895.2  962.8  908.6  908.0  888.9  833.7  933.7
#> 1997  840.9  727.4  857.9  849.0  994.8  830.2  971.1  836.0  939.1  976.9
#> 1998  917.3  716.2  822.9  970.1  970.2  849.4 1042.3  869.9  939.4 1021.3
#> 1999  942.0  738.4  903.2  953.2 1011.2  894.4 1054.5  899.5 1002.3 1043.7
#> 2000  924.9  798.2  901.9 1024.7 1052.3 1165.5  859.3 1009.2 1054.6 1070.4
#> 2001  971.9  814.6 1017.5 1039.2 1123.5 1024.9 1100.8  963.0 1012.9 1132.0
#> 2002 1027.9  841.4 1043.9 1075.3 1190.9 1143.0 1075.7 1065.9 1060.1 1211.4
#> 2003 1099.3  900.5 1092.7 1222.4 1237.1 1237.9 1182.0 1101.2 1198.2 1316.1
#> 2004 1182.9  989.8 1131.4 1277.1 1280.3 1384.1 1305.9 1166.8 1317.9 1358.3
#> 2005 1246.3 1037.3 1300.8 1153.7 1264.2 1454.2 1290.1 1210.7 1277.8 1314.4
#> 2006 1193.7 1037.7 1204.5 1348.6 1267.6 1429.0 1412.0 1239.2 1219.1 1344.6
#> 2007 1267.3 1047.0 1331.6 1302.6 1365.1 1491.5 1462.3 1315.5 1353.3 1440.6
#> 2008 1397.8 1140.5 1351.7 1396.6 1421.1 1401.6 1582.3 1268.4 1383.3 1452.4
#> 2009 1451.0 1056.6 1386.9 1509.1 1519.4 1500.5 1570.7 1341.5 1399.9 1534.3
#> 2010 1469.1 1111.9 1379.9 1389.7 1427.2 1551.4 1581.0 1324.0 1422.0 1464.9
#> 2011 1412.6 1117.5 1321.6 1472.6 1408.9 1471.9 1532.5 1293.5 1345.7 1404.7
#> 2012 1362.4 1131.7 1349.2 1391.2 1456.9 1616.4 1423.4 1359.0 1367.8 1442.6
#> 2013 1397.4 1113.6 1397.3 1339.1 1441.9 1537.4 1390.6 1337.2 1359.4 1463.3
#> 2014 1451.0 1064.9 1293.2 1442.9 1411.8 1461.6 1501.6 1254.2 1356.4 1478.7
#> 2015 1471.2 1053.8 1367.2 1442.2 1428.7 1480.9 1540.9 1331.9 1400.1 1566.3
#> 2016 1519.2 1155.8 1451.5 1451.0 1449.7 1596.1 1468.3 1293.9 1393.5 1497.4
#> 2017 1428.5 1092.4 1370.3 1522.6 1452.4 1557.2 1445.5 1303.1              
#>         Nov    Dec
#> 1982  522.9  889.3
#> 1983  568.7  963.7
#> 1984  623.2 1039.4
#> 1985  697.1 1187.5
#> 1986  700.9 1367.9
#> 1987  743.2 1460.1
#> 1988  870.3 1570.0
#> 1989  923.8 1569.4
#> 1990  931.5 1563.1
#> 1991  944.7 1600.3
#> 1992  968.6 1650.9
#> 1993  970.0 1710.5
#> 1994 1000.4 1817.5
#> 1995 1067.4 1857.2
#> 1996 1081.6 1837.6
#> 1997 1111.3 1879.1
#> 1998 1137.7 1975.7
#> 1999 1207.2 2069.6
#> 2000 1232.5 2177.5
#> 2001 1344.8 2269.5
#> 2002 1495.1 2338.6
#> 2003 1528.2 2424.2
#> 2004 1536.7 2500.8
#> 2005 1540.4 2536.0
#> 2006 1623.3 2611.1
#> 2007 1687.9 2747.0
#> 2008 1675.9 2886.1
#> 2009 1736.6 2795.1
#> 2010 1705.5 2752.4
#> 2011 1660.0 2730.5
#> 2012 1672.9 2753.3
#> 2013 1668.9 2725.5
#> 2014 1687.7 2756.9
#> 2015 1730.5 2913.6
#> 2016 1684.3 2850.4
#> 2017              
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
#> Tolerance: 1e-07
#> Exact ML: No
#> Unit root limit: 0.96
#> 
#> Transformation
#> Function: AUTO
#> AIC difference: 
#> Adjust: NONE
#> 
#> Regression
#> Calendar regressor: TD7
#> with Leap Year: Yes
#> AutoAdjust: FALSE
#> Test: TEST_NO
#> 
#> Easter: INCLUDEEASTER
#> 
#> Pre-specified outliers: 0
#> Ramps: No
#> User-defined variables: No
#> 
#> Outliers
#> Is enabled: No
#> 
#> ARIMA
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>         0         0 
#> 
#> Specification SEATS
#> Approximation mode: APP_LEGACY
#> MA unit root boundary: 0.95
#> Trend boundary: 0.5
#> Seasonal tolerance: 2
#> Seasonal boundary: 0.8
#> Method: ALG_BURMAN
#> 
#> Benchmarking
#> Is enabled: No
#> 
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
#> Tolerance: 1e-07
#> Exact ML: No
#> Unit root limit: 0.96
#> 
#> Transformation
#> Function: AUTO
#> AIC difference: 
#> Adjust: NONE
#> 
#> Regression
#> Calendar regressor: TD7
#> with Leap Year: Yes
#> AutoAdjust: FALSE
#> Test: TEST_NO
#> 
#> Easter: INCLUDEEASTER
#> 
#> Pre-specified outliers: 0
#> Ramps: No
#> User-defined variables: No
#> 
#> Outliers
#> Is enabled: No
#> 
#> ARIMA
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>         0         0 
#> 
#> Specification SEATS
#> Approximation mode: APP_LEGACY
#> MA unit root boundary: 0.95
#> Trend boundary: 0.5
#> Seasonal tolerance: 2
#> Seasonal boundary: 0.8
#> Method: ALG_BURMAN
#> 
#> Benchmarking
#> Is enabled: No
#> 
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
#> Tolerance: 1e-07
#> Exact ML: No
#> Unit root limit: 0.96
#> 
#> Transformation
#> Function: LOG
#> AIC difference: 
#> Adjust: NONE
#> 
#> Regression
#> Calendar regressor: TD7
#> with Leap Year: Yes
#> AutoAdjust: FALSE
#> Test: TEST_NO
#> 
#> Easter: INCLUDEEASTER
#> 
#> Pre-specified outliers: 2
#>  - AO (2000-06-01) 
#>  - AO (2000-07-01) 
#> Ramps: No
#> User-defined variables: No
#> 
#> Outliers
#> Is enabled: No
#> 
#> ARIMA
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>   -0.8278   -0.4255 
#> 
#> Specification SEATS
#> Approximation mode: APP_LEGACY
#> MA unit root boundary: 0.95
#> Trend boundary: 0.5
#> Seasonal tolerance: 2
#> Seasonal boundary: 0.8
#> Method: ALG_BURMAN
#> 
#> Benchmarking
#> Is enabled: No
#> 
#> 
#> $results
#> Model: TRAMO-SEATS
#> Log-transformation: yes 
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>   -0.8278   -0.4255 
#> 
#> Regression model:
#>             mon             tue             wed             thu             fri 
#>      -0.0109446       0.0048940       0.0001761       0.0132928      -0.0024801 
#>             sat              lp          easter AO (2000-06-01) AO (2000-07-01) 
#>       0.0153509       0.0410667       0.0503888       0.1681662      -0.1972348 
#> 
#> For a more detailed output, use the 'summary()' function.
#> 

# Writing the workspace
save_workspace(jws, file.path(dir, "workspace.xml"))
# }
```
