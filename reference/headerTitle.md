# Get header title

Get header title

## Usage

``` r
headerTitle(pkg_name, title = "", url_docu = NULL)
```

## Arguments

- pkg_name:

  name of the package

- title:

  title to display. If empty, the package name is used.

- url_docu:

  HTTP(S) base URL to package documentation, e.g.
  "https://pandora-isomemo.github.io/data-tools/". Must contain a
  hostname, with an optional port (0-65535) and path, but no query or
  fragment. If missing or invalid, the version is shown without a link.

## Value

A \[shiny::tagList()\]. If the package is not installed, only the
package name is shown.
