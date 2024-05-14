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
