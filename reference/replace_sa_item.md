# Replace or Remove a SA-item

`replace_sa_item()` replaces a SA-item in a SAProcessing and
`remove_sa_item()` removes a SA-item from a SAProcessing
`remove_all_sa_item()` removes all SA-item from a SAProcessing

## Usage

``` r
replace_sa_item(jsap, idx, jsai)

remove_sa_item(jsap, idx)

remove_all_sa_item(jsap)
```

## Arguments

- jsap:

  SAProcessing to be modified.

- idx:

  index of the target SA-item.

- jsai:

  new SA-item (for replacement).

## Value

`NULL` returned invisibly
