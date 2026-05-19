# Export to xlsx

Export a single data.frame. Or, export a list of data.frames into
workbook containing one data.frame in each sheet of a workbook. Sheet
names are taken from the names of the list.

## Usage

``` r
exportXLSX(file, dat)
```

## Arguments

- file:

  filename

- dat:

  (list) If `dat` is a data.frame `dat` will be written into a single
  sheet "Sheet 1". If `dat` is a named list of data.frames `dat` each
  data.frame is written into a separate sheet with sheet names taken
  from the names of the list.
