# Extract results from a SA-item

`.jsai_results()` extracts specific variables of the model of the
SA-item while `.jsai_jresults()` extracts the Java object of the results
of a SA-item.

## Usage

``` r
.jsai_results(jsai, items = NULL)

.jsai_jresults(jsai)
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
