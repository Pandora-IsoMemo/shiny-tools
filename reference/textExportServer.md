# Text Export Button

Backend for export module

## Usage

``` r
textExportButton(id, label = "Export")

textExportServer(id, outFun, filename)
```

## Arguments

- id:

  namespace id

- label:

  (character) button label

- outFun:

  (reactive) a reactive function returning the output for export, e.g.
  print, summary, ...

- filename:

  (character) name of file without file extension
