# Read a X13 specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
x13_read_spec(file)
```

## Arguments

- file:

  xml format,

## Value

list

## Examples

``` r
file <- system.file("workspaces", "workspace_test", "X13Spec",
                    "X13Spec-1.xml", package = "rjd3workspace")
my_spec<-x13_read_spec(file)
class(my_spec)
#> [1] "JD3_X13_SPEC"
str(my_spec)
#> List of 3
#>  $ regarima    :List of 7
#>   ..$ basic     :List of 3
#>   .. ..$ span            :List of 5
#>   .. .. ..$ type: chr "ALL"
#>   .. .. ..$ d0  : NULL
#>   .. .. ..$ d1  : NULL
#>   .. .. ..$ n0  : int 0
#>   .. .. ..$ n1  : int 0
#>   .. .. ..- attr(*, "class")= chr "JD3_SPAN"
#>   .. ..$ preprocessing   : logi TRUE
#>   .. ..$ preliminaryCheck: logi TRUE
#>   ..$ transform :List of 4
#>   .. ..$ fn      : chr "AUTO"
#>   .. ..$ adjust  : chr "NONE"
#>   .. ..$ aicdiff : num -2
#>   .. ..$ outliers: logi FALSE
#>   ..$ outlier   :List of 7
#>   .. ..$ outliers     :List of 3
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ type: chr "AO"
#>   .. .. .. ..$ va  : num 0
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ type: chr "LS"
#>   .. .. .. ..$ va  : num 0
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ type: chr "TC"
#>   .. .. .. ..$ va  : num 0
#>   .. ..$ span         :List of 5
#>   .. .. ..$ type: chr "ALL"
#>   .. .. ..$ d0  : NULL
#>   .. .. ..$ d1  : NULL
#>   .. .. ..$ n0  : int 0
#>   .. .. ..$ n1  : int 0
#>   .. .. ..- attr(*, "class")= chr "JD3_SPAN"
#>   .. ..$ defva        : num 0
#>   .. ..$ method       : chr "ADDONE"
#>   .. ..$ monthlytcrate: num 0.7
#>   .. ..$ maxiter      : int 30
#>   .. ..$ lsrun        : int 0
#>   ..$ arima     :List of 7
#>   .. ..$ period: int 0
#>   .. ..$ d     : int 1
#>   .. ..$ bd    : int 1
#>   .. ..$ phi   : NULL
#>   .. ..$ theta :List of 2
#>   .. .. ..$ : num 0
#>   .. .. ..$ : chr "UNDEFINED"
#>   .. .. ..- attr(*, "dim")= int [1:2] 2 1
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:2] "value" "type"
#>   .. .. .. ..$ : NULL
#>   .. ..$ bphi  : NULL
#>   .. ..$ btheta:List of 2
#>   .. .. ..$ : num 0
#>   .. .. ..$ : chr "UNDEFINED"
#>   .. .. ..- attr(*, "dim")= int [1:2] 2 1
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:2] "value" "type"
#>   .. .. .. ..$ : NULL
#>   .. ..- attr(*, "class")= chr "JD3_SARIMA_ESTIMATION"
#>   ..$ automodel :List of 12
#>   .. ..$ enabled  : logi TRUE
#>   .. ..$ ljungbox : num 0.95
#>   .. ..$ tsig     : num 1
#>   .. ..$ predcv   : num 0.143
#>   .. ..$ ubfinal  : num 1.05
#>   .. ..$ ub1      : num 1.03
#>   .. ..$ ub2      : num 1.14
#>   .. ..$ cancel   : num 0.1
#>   .. ..$ fct      : num 1.01
#>   .. ..$ acceptdef: logi FALSE
#>   .. ..$ mixed    : logi TRUE
#>   .. ..$ balanced : logi FALSE
#>   ..$ regression:List of 8
#>   .. ..$ mean         : NULL
#>   .. ..$ check_mean   : logi FALSE
#>   .. ..$ td           :List of 12
#>   .. .. ..$ td            : chr "TD_NONE"
#>   .. .. ..$ lp            : chr "NONE"
#>   .. .. ..$ holidays      : chr ""
#>   .. .. ..$ users         : chr(0) 
#>   .. .. ..$ w             : int 0
#>   .. .. ..$ test          : chr "NO"
#>   .. .. ..$ auto          : chr "AUTO_NO"
#>   .. .. ..$ autoadjust    : logi FALSE
#>   .. .. ..$ tdcoefficients: NULL
#>   .. .. ..$ lpcoefficient : NULL
#>   .. .. ..$ ptest1        : num 0
#>   .. .. ..$ ptest2        : num 0
#>   .. ..$ easter       :List of 4
#>   .. .. ..$ type       : chr "UNUSED"
#>   .. .. ..$ duration   : int 8
#>   .. .. ..$ test       : chr "NO"
#>   .. .. ..$ coefficient: NULL
#>   .. ..$ outliers     : NULL
#>   .. ..$ users        : NULL
#>   .. ..$ interventions: NULL
#>   .. ..$ ramps        : NULL
#>   ..$ estimate  :List of 2
#>   .. ..$ span:List of 5
#>   .. .. ..$ type: chr "ALL"
#>   .. .. ..$ d0  : NULL
#>   .. .. ..$ d1  : NULL
#>   .. .. ..$ n0  : int 0
#>   .. .. ..$ n1  : int 0
#>   .. .. ..- attr(*, "class")= chr "JD3_SPAN"
#>   .. ..$ tol : num 1e-07
#>   ..- attr(*, "class")= chr "JD3_REGARIMA_SPEC"
#>  $ x11         :List of 12
#>   ..$ mode         : chr "UNKNOWN"
#>   ..$ seasonal     : logi TRUE
#>   ..$ henderson    : int 0
#>   ..$ sfilters     : chr "FILTER_MSR"
#>   ..$ lsig         : num 1.5
#>   ..$ usig         : num 2.5
#>   ..$ nfcasts      : int -1
#>   ..$ nbcasts      : int 0
#>   ..$ sigma        : chr "NONE"
#>   ..$ vsigmas      : int(0) 
#>   ..$ excludefcasts: logi FALSE
#>   ..$ bias         : chr "RATIO"
#>   ..- attr(*, "class")= chr "JD3_X11_SPEC"
#>  $ benchmarking:List of 6
#>   ..$ enabled : logi FALSE
#>   ..$ target  : chr "TARGET_CALENDARADJUSTED"
#>   ..$ lambda  : num 1
#>   ..$ rho     : num 1
#>   ..$ bias    : chr "BIAS_NONE"
#>   ..$ forecast: logi FALSE
#>  - attr(*, "class")= chr "JD3_X13_SPEC"
```
