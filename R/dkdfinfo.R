# Load dataframes from system environment
getDataFrames = function() {
  ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) { is.data.frame(get(x)) })]
}

# Get information for dataframes
getdfinfo = function(dfn) {
  # mydf = dfn  # uncomment this line and comment the next line for testing
  mydf = get(dfn)
  fields = names(mydf)
  fields.type = sapply(fields, getVectorType, mydf)
  fields.numeric = fields[fields.type %in% c("numeric", "integer", "double", "logical")]
  fields.factor = fields[fields.type %in% c("factor", "binaryfactor")]
  fields.logical = fields[fields.type %in% c("logical", "binaryfactor")]
  fields.date = fields[fields.type %in% c("date")]
  # Convert columns to factor
  # print(fields.type)
  for (i in fields.factor) {
    mydf[, i] = as.factor(mydf[, i])
  }

  getdfinfo = list(
    dimensions = list(
      dim(mydf)[1], dim(mydf)[2]
    ),
    numerics = list(
      Variable = as.vector(fields.numeric),
      Missing = sapply(fields.numeric, function(x) { sum(is.na(mydf[, x])) }),
      Mean = sapply(fields.numeric, function(x) { round(mean(mydf[, x], na.rm = T), 2) }),
      Median = sapply(fields.numeric, function(x) { round(median(mydf[, x], na.rm = T), 2) }),
      SD = sapply(fields.numeric, function(x) { round(sd(mydf[, x], na.rm = T), 2) }),
      Minimum = sapply(fields.numeric, function(x) { min(mydf[, x], na.rm = T) }),
      Maximum = sapply(fields.numeric, function(x) { max(mydf[, x], na.rm = T) }),
      Plot = sapply(fields.numeric, function(x) { 
        spk_chr(
          values = round(mydf[, x], 2), 
          type = "box",
          raw = FALSE, 
          width = 50, height = 25
        )
      })
    ),
    factors = list(
      Variable = as.vector(fields.factor),
      Missing = sapply(fields.factor, function(x) { sum(is.na(mydf[, x])) }),
      Levels = sapply(fields.factor, function(x) { nlevels(mydf[, x]) }),
      Counts = sapply(fields.factor, function(x) {
        temp1 <- as.data.frame(table(mydf[, x]))
        temp2 <- apply(temp1, 1, function(x) paste0(trimws(x[1]), ": ", trimws(x[2])))
        paste(temp2, collapse = '<br />')
      }),
      Plot = sapply(fields.factor, function(x) {
        spk_chr(
          values = unname(table(as.numeric(mydf[, x]))), 
          type = "bar", 
          tooltipValueLookups = list(levels = levels(mydf[, x])), 
          tooltipFormat = '{{offset:levels}} : {{value}}', 
          width = 50, height = 25, barWidth = 20, barSpacing = 5, chartRangeMin = 0
        )
      })
    ),
    logicals = list(
      Variable = as.vector(fields.logical),
      Missing = sapply(fields.logical, function(x) { sum(is.na(mydf[, x])) }),
      PercentTrue = sapply(fields.logical, function(x) {
        xx = mydf[,x]
        xx = factor(xx)
        xx = sapply(levels(xx), function(x) as.integer(x == xx))[, 2]
        round(100 * mean(xx, na.rm = T), 2)
      })
    ),
    dates = list(
      Variable = as.vector(fields.date),
      Missing = sapply(fields.date, function(x) { sum(is.na(mydf[, x])) }),
      Minimum = sapply(fields.date, function(x) { min(mydf[, x], na.rm = T) }),
      Maximum = sapply(fields.date, function(x) { max(mydf[, x], na.rm = T) })
    )
  )
}

# Get numerical variables
getNumerics = function(dfn) {
  mydf = get(dfn)
  fields = names(mydf)
  fields.type = sapply(fields, getVectorType, mydf)
  fields.numeric = fields[fields.type %in% c("numeric", "integer", "double", "logical")]
  
  cbind(label = sapply(fields.numeric, function(x) { 
    sprintf("%s/Range: %.0f-%.0f | NAs: %.0f", x, min(mydf[, x], na.rm = T), max(mydf[, x], na.rm = T), sum(is.na(mydf[, x]))) 
  }))
}

getFactors = function(dfn) {
  mydf = get(dfn)
  fields = names(mydf)
  fields.type = sapply(fields, getVectorType, mydf)
  fields.factor = fields[fields.type %in% c("factor", "binaryfactor")]
  
  cbind(label = sapply(fields.factor, function(x) { 
    sprintf("%s/Levels: %.0f | NAs: %.0f", x, nlevels(factor(mydf[, x])), sum(is.na(mydf[, x])))
  }))
}

getDates = function(dfn) {
  mydf = get(dfn)
  fields = names(mydf)
  fields.type = sapply(fields, getVectorType, mydf)
  fields.date = fields[fields.type %in% c("date")]
  
  cbind(label = sapply(fields.date, function(x) {
    sprintf("%s/Min.: %s | Max.: %s | NAs: %.0f", x, min(mydf[, x], na.rm = T), max(mydf[, x], na.rm = T), sum(is.na(mydf[, x]))) 
  }))
}

getVectorType = function(field, mydf) {
  x = "Unknown"
  if (is.double(mydf[, field])) { 
    x = "double" 
  }
  if (is.character(mydf[, field])) { 
    x = "character" 
    if (length(unique(mydf[, field])) < 6) {
      mydf[, field] <- as.factor(mydf[, field]) # new
      x = "factor"
    }
  }
  if (is.integer(mydf[, field])) {
    x = "integer"
    if (sum(is.na(mydf[, field])) == 0 && max(mydf[, field]) == 1 && min(mydf[, field]) == 0) {
      x = "logical"
    }
    if (length(unique(mydf[, field])) < 6) {
      mydf[, field] <- as.factor(mydf[, field]) # new
      x = "factor"
    }
  }
  if (is.factor(mydf[, field])) { 
    mydf[, field] <- as.factor(mydf[, field]) # new
    x = "factor"
    if (nlevels(mydf[, field]) == 2) {
      mydf[, field] <- as.factor(mydf[, field]) # new
      x = "binaryfactor"
    }
  }
  # if (class(mydf[,field])[1] == "POSIXt") { x = "date" }
  if (inherits(mydf[, field], "Date") | inherits(mydf[, field], "POSIXt")) { 
    x = "date"
  }
  return(x)
}


