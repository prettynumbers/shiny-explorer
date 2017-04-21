shiny-explorer
==============

Shiny (www.rstudio.com/shiny) based data explorer with report templates based on field selection.

![Screenshot](screenshots.jpg)

## Instructions

From github:

```
library(shiny)
runGitHub("shiny-explorer","dkilfoyle")
```

From downloaded source:

```
shiny:::runApp("../shiny-explorer")
```

## Template Processing

All templates undergo (in order):
* gsub for mydf, numeric, factor1 etc
* brew
* knit2html

## Dependencies

* shiny >= 0.11 (bootstrap3)
* shinyAce
* tabplot
* gmodels
* vcd