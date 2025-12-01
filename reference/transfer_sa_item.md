# Copy & paste SA-items from one `SA-Processing` to another

Copy & paste SA-items from one `SA-Processing` to another

## Usage

``` r
transfer_sa_item(
  jsap_from,
  jsap_to,
  selected_sa_items,
  print_indications = TRUE
)
```

## Arguments

- jsap_from:

  SA-Processing from which to take the SA-items

- jsap_to:

  SA-Processing to which paste the SA-items

- selected_sa_items:

  vector containing the SA-items names to be updated.

- print_indications:

  A boolean to print indications on the processing status (optional)

## Value

`NULL` returned invisibly

## Details

If `selected_sa_items` is missing, all SA-items from `jsap_from` will be
copied.
