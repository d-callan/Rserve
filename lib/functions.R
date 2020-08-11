## TODO reorganize these, maybe helpers into a separate file ??

getAggStr <- function(col, group, panel) {
  aggStr <- paste(c(col, paste(c(group,panel), collapse=" + ")), collapse=" ~ ")

  return(aggStr)
}


## TODO optimize aggregate calls, probably with data.table syntax
# data is a data.table
# col is a column name we want a summary for
# group is a column name for the aggregation key within a panel
# panel is a column name for the strata/ facet
groupSummary <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(round(quantile(data[[col]]),4)))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(quantile(x),4)}))
  }

  colNames <- c(group, panel, 'min', 'q1', 'median', 'q3', 'max')
  names(dt) <- colNames

  return(dt)
}


groupMean <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(round(mean(data[[col]],4)))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(mean(x),4)}))
  }

  colNames <- c(group, panel, 'mean')
  names(dt) <- colNames

  return(dt)
}


groupSD <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(round(sd(data[[col]],4)))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, FUN = function(x){round(sd(x),4)}))
  }

  colNames <- c(group, panel, 'sd')
  names(dt) <- colNames

  return(dt)
}


groupSize <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(length(data[[col]])))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, length))
  }

  colNames <- c(group, panel, 'size')
  names(dt) <- colNames

  return(dt)
}


outliers <- function(x) {
  summary <- quantile(x)
  iqr <- summary[4] - summary[2]
  lowCutoff <- summary[2] - (1.5*iqr)
  highCutoff <- summary[4] + (1.5*iqr)

  return(x[x < lowCutoff | x > highCutoff])
}

groupOutliers <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- as.data.table(t(outliers(data[[col]])))
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, outliers))
  }

  colNames <- c(group, panel, 'outliers')
  names(dt) <- colNames

  return(dt)
}


## TODO consider renaming x and y to independent and dependent
densityCurve <- function(x) {
  curve <- density(x)
  
  return(data.table("x" = list(curve$x), "y" = list(curve$y)))
}

groupDensity <- function(data, col, group = NULL, panel = NULL) {
  aggStr <- getAggStr(col, group, panel)

  if (aggStr == col) {
    dt <- densityCurve(data[[col]])
  } else {
    dt <- as.data.table(aggregate(as.formula(aggStr), data, densityCurve))
  }

  colNames <- c(group, panel, 'x', 'y')
  names(dt) <- colNames

  return(dt)
}


# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

data_frame <- function(...) {
  new_data_frame(list(...))
}

predictdf.loess <- function(model, xseq, se = TRUE, level = .95) {
  pred <- stats::predict(model, newdata = data_frame(x = xseq), se = se)

  if (se) {
    y = pred$fit
    ci <- pred$se.fit * stats::qt(level / 2 + .5, pred$df)
    ymin = y - ci
    ymax = y + ci
    base::data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)
  } else {
    base::data.frame(x = xseq, y = as.vector(pred))
  }
}

predictdf.gam <- function(model, xseq, se = TRUE, level = .95) {
  pred <- stats::predict(model, newdata = data_frame(x = xseq), se.fit = se,
    type = "link")

  if (se) {
    std <- stats::qnorm(level / 2 + 0.5)
    base::data.frame(
      x = xseq,
      y = model$family$linkinv(as.vector(pred$fit)),
      ymin = model$family$linkinv(as.vector(pred$fit - std * pred$se.fit)),
      ymax = model$family$linkinv(as.vector(pred$fit + std * pred$se.fit)),
      se = as.vector(pred$se.fit)
    )
  } else {
    base::data.frame(x = xseq, y = model$family$linkinv(as.vector(pred)))
  }
}

# assumes that dt has an x and y column
smoothedMean <- function(dt, method) {
  xseq <- sort(unique(dt$x))

  if (method == 'loess') {
    smoothed <- loess(y ~ x, dt)
    smoothed <- as.data.table(predictdf.loess(smoothed, xseq))
  } else if (method == 'gam') {
    smoothed <- mgcv::gam(y ~ s(x, bs = "cs"), data = dt, method = "REML")
    smoothed <- as.data.table(predictdf.gam(smoothed, xseq))
  } else {
    stop('Unrecognized smoothing method.')
  }

  return(data.table("x" = list(dt$x), "y" = list(dt$y), "ymin" = list(dt$ymin), "ymax" = list(dt$ymax), "se" = list(dt$se)))
}

## TODO optimize !!
groupSmoothedMean <- function(data, x, y, group = NULL, panel = NULL) {
  names(data)[names(data) == y] <- 'y'
  names(data)[names(data) == x] <- 'x'
  y <- 'y'
  x <- 'x'
  aggStr <- paste0("x + ", getAggStr(y, group, panel))

  maxGroupSize <- max(groupSize(data, col, group, panel)$size)
  method <- 'loess'
  if (maxGroupSize > 1000) { method <- 'gam' }


  if (aggStr == col) {
    dt <- smoothedMean(data, method)
  } else {
    dt.list <- split(data, list(data[[group]], data[[panel]]))
    dt.list <- lapply(dt.list, smoothedMean, method)
    dt <- reduce(dt.list, rbind)
    dt$name <- names(dt.list)
    dt$group <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 1))
    dt$panel <- unlist(lapply(strsplit(dt$name, ".", fixed=T), "[", 2))
    dt$name <- NULL
  }


}  
