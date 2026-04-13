# Deprecated functions

Deprecated functions

## Usage

``` r
.jmp_sa_count(jmp)

.jsap_sa_count(jmp)

.jsap_sai_count(jsap)

.jmp_name(jmp)

.jsap_name(jsap)

.jmp_sa(jmp, idx)

.jsap_sa(jsap, idx)

.jsap_sai(jsap, idx)

.jmp_sa_name(jmp)

.jsap_sa_name(jsap)

.jsap_sai_names(jsap)

.jmp_load(jmp)

.jsa_read(jsa)

.jsa_results(jsa, items = NULL)

.jsa_jresults(jsa)

.jsa_metadata(jsa, key)

.jsai_metadata(jsai, key)

.jsa_ts_metadata(jsa, key)

.jsai_ts_metadata(jsa, key)

.jws_sap_count(jws)

.jws_open(file)

.jread_workspace(jws, compute = TRUE)

.jread_sap(jsap)

.jws_new(modelling_context = NULL)

.jws_sap_new(jws, name)

.jws_make_copy(jws)

.jsap_make_copy(jsap)

.jws_compute(jws)

.jws_sap(jws, idx)

.jsai_name(jsai)

.jsap_refresh(
  jsap,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"),
  period = 0,
  start = NULL,
  end = NULL,
  info = c("All", "Data", "None")
)

.jws_refresh(
  jws,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"),
  period = 0,
  start = NULL,
  end = NULL,
  info = c("All", "Data", "None")
)

transfer_series(
  jsap_from,
  jsap_to,
  selected_sa_items,
  print_indications = TRUE
)

.jws_add(jws, jsap)

set_domain_specification(jsap, idx, spec)

get_domain_specification(jsai)

get_point_specification(jsai)
```

## Arguments

- jmp, idx, jws, name, jsa, jsai, jsap, items, key, file, compute,
  policy, period, start, end, info, modelling_context, jsap_from,
  jsap_to, selected_sa_items, print_indications, spec:

  Parameters.

## Value

The same value as returned by the corresponding non-deprecated function.
The returned object represents an encoded identifier for a spreadsheet
series or collection.
