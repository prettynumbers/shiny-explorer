shiny-explorer
==============

Shiny (www.rstudio.com/shiny) based data explorer with report templates based on field selection.

![Screenshot](screenshots.jpg)

## Instructions

First, install necessary packages:

```
list.of.packages <- as.character(expression(knitr, ggplot2, shiny, shinyAce, htmlwidgets, readxl, sqldf, DT, sparkline, brew, plyr, highr, rpivotTable, survival, lme4, gmodels, gplots, gridExtra, vcd, ez))
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

Run from GitHub:

```
shiny::runGitHub(repo = "shiny-explorer", username = "prettynumbers")
```

Run from downloaded source:

```
shiny:::runApp("../shiny-explorer")
```

## Template Processing

All templates undergo (in order):
* gsub for mydf, numeric, factor1, etc.
* brew
* knit2html

## Dependencies

* shiny
* shinyAce
* tabplot
* gmodels
* vcd
* ...