# Server function for data export

Backend for data export module

## Usage

``` r
dataExportServer(id, dataFun, filename = "data")
```

## Arguments

- id:

  namespace id

- dataFun:

  (reactive) a reactive function returning a data.frame for export

- filename:

  (character) name of file without file extension
