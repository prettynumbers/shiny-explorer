library(shiny)
library(shinyAce)
require(rpivotTable)  # install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))

data(iris)
data(airquality)

clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}

# map.df <- readRDS("/Users/khanoa/box/VMAC BIOSTAT/DATA/MAP/mergedData/MAP_bh_d20180301_m20180301.rds")
MAP_WMH_Descriptive_20180322 <- readRDS("/Users/khanoa/box/VMAC BIOSTAT/MAP WMH Descriptive (AJ)/data/MAP_WMH_Descriptive_20180322.rds")
MAP_WMH_Descriptive_20180322 <- clear.labels(MAP_WMH_Descriptive_20180322)
# map.df <- map.df[map.df$epoch == 1, c("map.id", "epoch", "enrolled.dx.factor", grep("^np.*", names(map.df), v = T)[1:20], grep("^csf.*", names(map.df), v = T)[1:20]), ]
MAP_WMH_Descriptive_20180322[, grep("date|notes", names(MAP_WMH_Descriptive_20180322), v = T)] <- NULL
