library(shiny)
library(shinyAce)
require(rpivotTable) #install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))
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

map.df <- readRDS("/Users/khanoa/box/VMAC BIOSTAT/DATA/MAP/mergedData/MAP_bh_d20180301_m20180301.rds")
map.df <- map.df[, c("map.id", "epoch", "enrolled.dx.factor", grep("^np.*", names(map.df), v = T)[1:20], grep("^csf.*", names(map.df), v = T)[1:20]), ]
map.df <- clear.labels(map.df)
map.df[, grep("date|notes", names(map.df), v = T)] <- NULL
