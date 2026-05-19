# Changelog

## shinyTools 26.05.0

### Updates

- Robustified test for more recent ggplot versions

## shinyTools 25.09.0

### Bug Fixes

- fixed an issue with incorrect color mapping when joining data for
  legend.

## shinyTools 25.07.0

### Bug Fixes

- *customPoints* module: fixed an issue with missing group variable in a
  layer for density or histogram plots

## shinyTools 25.06.0

### New Features

- *customPoints* module: option to add custom points to ggplots with a
  discrete x-scale (boxplot, …)

## shinyTools 25.04.0

### Bug Fixes

- *headerUI*: icons instead of full logos for links in app headers (#39)

## shinyTools 25.02.0

### New Features

- *plotLegend* module:
  - option to set orientation and individual labels for the legend (#37)
  - new function
    ([`setLegendThemeOfGGplot()`](https://pandora-isomemo.github.io/shiny-tools/reference/setLegendThemeOfGGplot.md))
    to easily set the theme given the output of *plotLegendServer*
- *plotTitles* module: removed ‘legend’ from select choices, legend
  layout must be set with the *plotLegend* module now

## shinyTools 25.01.0

### New Features

- new module (`customPoints`) that allows a user to add points to a plot
  and format them (#35)

### Updates

- *headerUI*: keep only logos for *Pandora* and *Isomemo*, add button to
  *Data Search* app (#1)

## shinyTools 24.12.0

### Updates

- *headerUI*: integration of new links and logos (#1)

### Bug Fixes

- fix folder name of resource path

## shinyTools 24.11.2

### Updates

- *shinyTryCatch*: option to suppress alerts for warnings

### Bug Fixes

- *plotExport*:
  - fix issue with the assignment of the *InitRanges* argument to
    reactive values
  - fix error when plotting axis with discrete data

## shinyTools 24.11.1

### Updates

- *headerButtons*:
  - replacing path to logos with a URL
  - adding URL to a favicon for the browser tab

## shinyTools 24.11.0

### Bug Fixes

- fix issue with permanently disabled button in the *plotExport* module
  for base plot objects

## shinyTools 24.10.1

### New Features

- new module *textExport* for exporting output from print,
  capture.output or other text as a text file

### Updates

- enable filename input to be reactive in the modules *dataExport* and
  *plotExport*

## shinyTools 24.10.0

### New Features

- *plotTitles* module (#28):
  - option to format a second y-axis
  - option to use transformations for the x or y axis in order to handle
    extreme values

### Updates

- removing documentation from `man` folder for helper functions to clean
  up the reference page
  (<https://pandora-isomemo.github.io/shiny-tools/reference/index.html>)

### Bug Fixes

- fix typo in example for ‘mathematical annotation’ in the plotTitles
  module

## shinyTools 24.08.1

### New Features

- *plotTitles* module: optionally, use notation for sub- and
  superscripts in titles of a plots, axis or the legend (#27)

## shinyTools 24.08.0

### New Features

- new UI for legends (specify the position of a legend)

### Updates

- update to the UI and logic to set custom titles of plots

## shinyTools 24.05.3

### New Features

- *plotTitles* module: optionally, change the font family of titles and
  axis texts

## shinyTools 24.05.2

### New Features

- *plotTitles* module: optionally, change the angle, hjust or vjust of
  axis texts

## shinyTools 24.05.1

### New Features

- catch *multiple errors and warnings* (#20):
  - shifted the function `DataTools::tryCatchWithWarningsAndErrors()` to
    this package
  - renamed function to
    [`shinyTryCatch()`](https://pandora-isomemo.github.io/shiny-tools/reference/shinyTryCatch.md)
  - updated the logic such that now all errors/warnings are caught and
    displayed in the app

## shinyTools 24.05.0

### New Features

- optionally, specify the tag of a modules title
- new input module that allows a user to update a numeric vector

## shinyTools 24.04.2

### New Features

- option to use named lists of data.frames in the dataExport module
  - for `xlsx` export, each data.frame is written into a separate sheet
    with sheet names taken from the names of the list

## shinyTools 24.04.1

### New Features

- module *plotTitles*:
  - optionally specify elements that can be selected from the ‘Label’
    input. Can be one or several of `"title", "axis", "legend"`
    elements.
- module *dataExport*:
  - disable button if no data
- module *plotExport*:
  - disable button if no data
  - optionally hide the range UI but show the titles UI in the
    *plotExport* popup.
- module *plotPoints*:
  - optionally hide (disable) specific inputs in the UI. E.g. if `alpha`
    should be set outside of this module

## shinyTools 24.04.0

### New Features

- new module plotPoints, and new function
  [`formatPointsOfGGplot()`](https://pandora-isomemo.github.io/shiny-tools/reference/formatPointsOfGGplot.md)
  for formatting points of ggplots

## shinyTools 24.03.3

### New Features

- module for plotExport (#6)
  - option to add and format titles of a plot before export if the plot
    is a ggplot
  - option to set ranges of a plot before export if the plot is a ggplot

## shinyTools 24.03.2

### New Features

- module for plotExport (#6)
  - export a plot as “png”, “jpeg”, “svg”, “pdf”, or “tiff” file
  - the module was extracted from PlotR, resources
  - it can be applied in all new apps

## shinyTools 24.03.0

### New Features

- module for dataExport (#4)
  - export a data.frame as xlsx, csv, or json file
  - the module was extracted from MapR, iso-app
  - it can be applied in all new apps

## shinyTools 24.02.0

### New Features

- headerButtons module applicable across apps
