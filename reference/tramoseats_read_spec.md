# Read a Tramo-Seats specification file

The specification file is a xml file like the one JDemetra+ would write
when defining a specification in the Graphical User Interface.

## Usage

``` r
tramoseats_read_spec(file)
```

## Arguments

- file:

  xml format,

## Value

list

## Examples

``` r
file <- system.file("workspaces", "workspace_test", "TramoSeatsSpec",
                    "TramoSeatsSpec-1.xml", package = "rjd3workspace")
my_spec<- tramoseats_read_spec(file)
class(my_spec)
#> [1] "JD3_TRAMOSEATS_SPEC"
str(my_spec)
#> List of 3
#>  $ tramo       :List of 7
#>   ..$ basic     :List of 3
#>   .. ..$ span            :List of 5
#>   .. .. ..$ type: chr "ALL"
#>   .. .. ..$ d0  : NULL
#>   .. .. ..$ d1  : NULL
#>   .. .. ..$ n0  : int 0
#>   .. .. ..$ n1  : int 0
#>   .. .. ..- attr(*, "class")= chr "JD3_SPAN"
#>   .. ..$ preliminaryCheck: logi TRUE
#>   .. ..$ frequency       : int -1
#>   ..$ transform :List of 4
#>   .. ..$ fn      : chr "AUTO"
#>   .. ..$ fct     : num 0.95
#>   .. ..$ adjust  : chr "NONE"
#>   .. ..$ outliers: logi FALSE
#>   ..$ outlier   :List of 9
#>   .. ..$ enabled: logi FALSE
#>   .. ..$ span   :List of 5
#>   .. .. ..$ type: chr "ALL"
#>   .. .. ..$ d0  : NULL
#>   .. .. ..$ d1  : NULL
#>   .. .. ..$ n0  : int 0
#>   .. .. ..$ n1  : int 0
#>   .. .. ..- attr(*, "class")= chr "JD3_SPAN"
#>   .. ..$ ao     : logi FALSE
#>   .. ..$ ls     : logi FALSE
#>   .. ..$ tc     : logi FALSE
#>   .. ..$ so     : logi FALSE
#>   .. ..$ va     : num 0
#>   .. ..$ tcrate : num 0.7
#>   .. ..$ ml     : logi FALSE
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
#>   ..$ automodel :List of 9
#>   .. ..$ enabled   : logi TRUE
#>   .. ..$ acceptdef : logi FALSE
#>   .. ..$ cancel    : num 0.05
#>   .. ..$ ub1       : num 0.97
#>   .. ..$ ub2       : num 0.91
#>   .. ..$ pcr       : num 0.95
#>   .. ..$ pc        : num 0.12
#>   .. ..$ tsig      : num 1
#>   .. ..$ amicompare: logi FALSE
#>   ..$ regression:List of 8
#>   .. ..$ mean         : NULL
#>   .. ..$ check_mean   : logi FALSE
#>   .. ..$ td           :List of 11
#>   .. .. ..$ td            : chr "TD7"
#>   .. .. ..$ lp            : chr "LEAPYEAR"
#>   .. .. ..$ holidays      : chr ""
#>   .. .. ..$ users         : chr(0) 
#>   .. .. ..$ w             : int 0
#>   .. .. ..$ test          : chr "TEST_NO"
#>   .. .. ..$ auto          : chr "AUTO_FTEST"
#>   .. .. ..$ ptest         : num 0.01
#>   .. .. ..$ autoadjust    : logi FALSE
#>   .. .. ..$ tdcoefficients: NULL
#>   .. .. ..$ lpcoefficient : NULL
#>   .. ..$ easter       :List of 5
#>   .. .. ..$ type       : chr "INCLUDEEASTER"
#>   .. .. ..$ duration   : int 6
#>   .. .. ..$ julian     : logi FALSE
#>   .. .. ..$ test       : logi TRUE
#>   .. .. ..$ coefficient: NULL
#>   .. ..$ outliers     : NULL
#>   .. ..$ users        : NULL
#>   .. ..$ interventions: NULL
#>   .. ..$ ramps        : NULL
#>   ..$ estimate  :List of 4
#>   .. ..$ span:List of 5
#>   .. .. ..$ type: chr "ALL"
#>   .. .. ..$ d0  : NULL
#>   .. .. ..$ d1  : NULL
#>   .. .. ..$ n0  : int 0
#>   .. .. ..$ n1  : int 0
#>   .. .. ..- attr(*, "class")= chr "JD3_SPAN"
#>   .. ..$ ml  : logi TRUE
#>   .. ..$ tol : num 1e-07
#>   .. ..$ ubp : num 0.96
#>   ..- attr(*, "class")= chr "JD3_TRAMO_SPEC"
#>  $ seats       :List of 10
#>   ..$ xl           : num 0.95
#>   ..$ approximation: chr "APP_LEGACY"
#>   ..$ epsphi       : num 2
#>   ..$ rmod         : num 0.5
#>   ..$ sbound       : num 0.8
#>   ..$ sboundatpi   : num 0.8
#>   ..$ bias         : logi TRUE
#>   ..$ nfcasts      : int -2
#>   ..$ nbcasts      : int 0
#>   ..$ algorithm    : chr "ALG_BURMAN"
#>   ..- attr(*, "class")= chr "JD3_SEATS_SPEC"
#>  $ benchmarking:List of 6
#>   ..$ enabled : logi FALSE
#>   ..$ target  : chr "TARGET_CALENDARADJUSTED"
#>   ..$ lambda  : num 1
#>   ..$ rho     : num 1
#>   ..$ bias    : chr "BIAS_NONE"
#>   ..$ forecast: logi FALSE
#>  - attr(*, "class")= chr "JD3_TRAMOSEATS_SPEC"
```
