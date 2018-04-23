# Load dataframes from system environment
getDataFrames = function()
{
  ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x){is.data.frame(get(x))})]
}

# Get information for dataframes
getdfinfo = function(dfn)
{
  mydf = dfn  # uncomment this line and comment the next line for testing
  # mydf = get(dfn)
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
    numerics = list(
      Variable = as.vector(fields.numeric),
      NAs = sapply(fields.numeric, function(x) { sum(is.na(mydf[, x])) }),
      Mean = sapply(fields.numeric, function(x) { round(mean(mydf[, x], na.rm = T), 2) }),
      Median = sapply(fields.numeric, function(x) { round(median(mydf[, x], na.rm = T), 2) }),
      SD = sapply(fields.numeric, function(x) { round(sd(mydf[, x], na.rm = T), 2) }),
      Minimum = sapply(fields.numeric, function(x) { min(mydf[, x], na.rm = T) }),
      Maximum = sapply(fields.numeric, function(x) { max(mydf[, x], na.rm = T) }),
      spark = sapply(fields.numeric, function(x) { spk_chr(mydf[, x], type = "box") })
      # ,
      # spark = sapply(fields.numeric, function(x) {
      #   boxstats = boxplot.stats(mydf[,x], do.out = T)
      #   paste(span(class="sparkline-line", values = paste(boxstats$stats, sep = "", collapse = ",")))
      # })
    ),
    factors = list(
      Variable = as.vector(fields.factor),
      NAs = sapply(fields.factor, function(x) { sum(is.na(mydf[, x])) }),
      Levels = sapply(fields.factor, function(x) { nlevels(mydf[, x]) }),
      spark = sapply(fields.factor, function(x) {
        barstats = table(mydf[, x])
        #barstats = hist(mydf[, x], plot = FALSE, breaks = length(unique(mydf[, x])))$counts
        spk_chr(barstats, type = "bar")
      })
      # ,
      # spark =sapply(fields.factor, function(x) {
      #   barstats = hist(as.numeric(mydf[,x]), plot=FALSE, breaks = length(unique(mydf[,x])))$counts
      #   paste(span(class="sparkline-bar", values = paste(barstats, sep = "", collapse = ",")))
      # })
    ),
    logicals = list(
      Variable = as.vector(fields.logical),
      NAs = sapply(fields.logical, function(x) { sum(is.na(mydf[, x])) }),
      Mean = sapply(fields.logical, function(x) {
        xx = mydf[,x]
        xx = factor(xx)
        xx = sapply(levels(xx), function(x) as.integer(x == xx))[, 2]
        mean(xx, na.rm = T)
      })),
    dates = list(
      Variable = as.vector(fields.date),
      NAs = sapply(fields.date, function(x) { sum(is.na(mydf[, x])) })),
      Minimum = sapply(fields.date, function(x) { min(mydf[, x], na.rm = T) }),
      Maximum = sapply(fields.date, function(x) { max(mydf[, x], na.rm = T) })
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
    sprintf("%s/nlevels=%.0f NAs=%.0f", x, nlevels(mydf[, x]), sum(is.na(mydf[, x])))
  }))
}

getDates = function(dfn) {
  mydf = get(dfn)
  fields = names(mydf)
  fields.type = sapply(fields, getVectorType, mydf)
  fields.date = fields[fields.type %in% c("date")]
  
  cbind(label = sapply(fields.date, function(x) {
    sprintf("%s/min=%s max=%s NAs=%.0f", x, min(mydf[, x], na.rm = T), max(mydf[, x], na.rm = T), sum(is.na(mydf[, x]))) 
  }))
}

getVectorType = function(field, mydf) {
  x = "Unknown"
  if (is.double(mydf[, field])) { 
    x = "double" 
  }
  if (is.character(mydf[, field])) { 
    x = "character" 
    # if (length(unique(mydf[, field])) < 20) {
    if (is.factor(mydf[, field])) {
      x = "factor"
    }
  }
  if (is.integer(mydf[, field])) {
    x = "integer"
    if (sum(is.na(mydf[, field])) == 0 && max(mydf[, field]) == 1 && min(mydf[, field]) == 0) {
      x = "logical"
    }
    if (length(unique(mydf[, field])) < 4) {
      mydf[, field] <- as.factor(mydf[, field]) # new
      x = "factor"
    }
  }
  if (is.factor(mydf[, field])) { 
    x = "factor"
    if (nlevels(mydf[, field]) == 2) {
      x = "binaryfactor"
    }
  }
  # if (class(mydf[,field])[1] == "POSIXt") { x = "date" }
  if (inherits(mydf[, field], "Date")) { 
    x = "date"
  }
  return(x)
}


