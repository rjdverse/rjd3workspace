# Extract results from a SA-item

`get_results()` extracts the results of a SA-item.
[`.jsai_results()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
extracts specific output of the model of the SA-item.
[`.jsai_jresults()`](https://rjdverse.github.io/rjd3workspace/reference/deprecated-rjd3workspace.md)
extracts the Java object of the results of a SA-item.

## Usage

``` r
get_results(jsai)

jsai_results(jsai, items = NULL)

jsai_jresults(jsai)
```

## Arguments

- jsai:

  Java SA-item object.

- items:

  vector of characters containing the variables to extract. See
  [`rjd3x13::x13_dictionary()`](https://rjdverse.github.io/rjd3x13/reference/x13_dictionary.html)
  or
  [`rjd3tramoseats::tramoseats_dictionary()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_dictionary.html).
  By default, extracts all the possible variables.

## Value

List with all the results of the adjustment.
