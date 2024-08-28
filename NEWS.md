# shinyTools 24.08.1

## New Features
- _plotTitles_ module: optionally, use notation for sub- and superscripts in titles of a plots, axis
  or the legend

# shinyTools 24.08.0

## New Features
- new UI for legends (specify the position of a legend)

## Updates
- update to the UI and logic to set custom titles of plots

# shinyTools 24.05.3

## New Features
- _plotTitles_ module: optionally, change the font family of titles and axis texts

# shinyTools 24.05.2

## New Features
- _plotTitles_ module: optionally, change the angle, hjust or vjust of axis texts

# shinyTools 24.05.1

## New Features
- catch _multiple errors and warnings_ (#20):
  - shifted the function `DataTools::tryCatchWithWarningsAndErrors()` to this package
  - renamed function to `shinyTryCatch()`
  - updated the logic such that now all errors/warnings are caught and displayed in the app

# shinyTools 24.05.0

## New Features
- optionally, specify the tag of a modules title
- new input module that allows a user to update a numeric vector

# shinyTools 24.04.2  

## New Features
- option to use named lists of data.frames in the dataExport module
  - for `xlsx` export, each data.frame is written into a separate sheet with sheet names taken from
    the names of the list

# shinyTools 24.04.1

## New Features  
- module _plotTitles_:
  - optionally specify elements that can be selected from the 'Label' input. Can 
  be one or several of `"title", "axis", "legend"` elements.
- module _dataExport_:
  - disable button if no data
- module _plotExport_: 
  - disable button if no data
  - optionally hide the range UI but show the titles UI in the _plotExport_ popup.
- module _plotPoints_:
  - optionally hide (disable) specific inputs in the UI. E.g. if `alpha` should be set outside of
    this module

# shinyTools 24.04.0

## New Features
- new module plotPoints, and new function `formatPointsOfGGplot()` for formatting points of ggplots

# shinyTools 24.03.3

## New Features
- module for plotExport (#6)
  - option to add and format titles of a plot before export if the plot is a ggplot
  - option to set ranges of a plot before export if the plot is a ggplot

# shinyTools 24.03.2

## New Features
- module for plotExport (#6)
  - export a plot as "png", "jpeg", "svg", "pdf", or "tiff" file
  - the module was extracted from PlotR, resources
  - it can be applied in all new apps

# shinyTools 24.03.0

## New Features
- module for dataExport (#4)
  - export a data.frame as xlsx, csv, or json file
  - the module was extracted from MapR, iso-app
  - it can be applied in all new apps

# shinyTools 24.02.0

## New Features
- headerButtons module applicable across apps
