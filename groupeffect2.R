
groupeffect <- function(data, groupvar, targetvar, model, strata = NULL, nreplications = 100, ...) {
  # Assign each member in the dataset a random group sampled from teh distribution
  # of groups in the dataset. The goal is to make each record member of the average
  # group. We then calculate predictions using the model. We repeat a number of
  # times to average out the sampling
  p <- lapply(seq_len(nreplications), function(i, ...) {
    tmp <- data
    tmp[[groupvar]] <- sample(data[[groupvar]])
    p <- predict(model, newdata = tmp, ...)
    aggregate(p, subset(data, select = c(groupvar, strata)), mean)
  }, ...)
  p <- do.call(rbind, p) 
  p <- aggregate(p$x, subset(p, select = c(groupvar, strata)), mean)
  names(p)[names(p) == "x"] <- "ygroupcorrected"
  # Calculatre the group average and compate to the overall mean
  tab <- aggregate(data.frame(y = data[[targetvar]]), 
    subset(data, select = c(groupvar, strata)), mean)
  if (!is.null(strata) && length(strata)) {
    ymean <- aggregate(data.frame(ymean = data[[targetvar]]), 
      subset(data, select = c(strata)), mean)
    tab <- merge(tab, ymean, by = strata, all = TRUE)
  } else tab$ymean <- mean(data[[targetvar]])
  tab$diff  <- tab$y - tab$ymean
  # Using the previous predictions: calculate the group effect
  tab <- merge(tab, p, by = c(groupvar, strata), all = TRUE)
  tab$groupeffect <- tab$y - tab$ygroupcorrected
  tab$xeffect     <- tab$ygroupcorrected - tab$ymean
  tab
}

groupeffect2 <- function(data, groupvar, targetvar, model, strata = NULL, nreplications = 100, ...) {
  vars <- formula(model) |> all.vars() |> setdiff(c(groupvar, targetvar))
  vars <- setdiff(vars, strata)
  # Assign each member in the dataset a random group sampled from teh distribution
  # of groups in the dataset. The goal is to make each record member of the average
  # group. We then calculate predictions using the model. We repeat a number of
  # times to average out the sampling
  p <- lapply(seq_len(nreplications), function(i, ...) {
    tmp <- data
    i <- sample(nrow(data))
    for (var in vars)
      tmp[[var]] <- data[[var]][i]
    p <- predict(model, newdata = tmp, ...)
    aggregate(p, subset(data, select = c(groupvar, strata)), mean)
  }, ...)
  p <- do.call(rbind, p) 
  p <- aggregate(p$x, subset(p, select = c(groupvar, strata)), mean)
  names(p)[names(p) == "x"] <- "yxcorrected"
  # Calculatre the group average and compate to the overall mean
  tab <- aggregate(data.frame(y = data[[targetvar]]), 
    subset(data, select= c(groupvar, strata)), mean)
  if (!is.null(strata) && length(strata)) {
    ymean <- aggregate(data.frame(ymean = data[[targetvar]]), 
      subset(data, select = c(strata)), mean)
    tab <- merge(tab, ymean, by = strata, all = TRUE)
  } else tab$ymean <- mean(data[[targetvar]])
  tab$diff  <- tab$y - tab$ymean
  # Using the previous predictions: calculate the group effect
  tab <- merge(tab, p, by = c(groupvar,strata), all = TRUE)
  tab$groupeffect <- tab$yxcorrected - tab$ymean
  tab$xeffect     <- tab$y - tab$yxcorrected
  tab
}


groupeffect3 <- function(data, groupvar, targetvar, model, strata = NULL, nreplications = 100, ...) {
  vars <- formula(model) |> all.vars() |> setdiff(c(groupvar, targetvar))
  vars <- setdiff(vars, strata)
  # Calculate ymean corrected for group
  pgroup <- lapply(seq_len(nreplications), function(i, ...) {
    tmp <- data
    tmp[[groupvar]] <- sample(data[[groupvar]])
    p <- predict(model, newdata = tmp, ...)
    aggregate(p, subset(data, select = c(groupvar, strata)), mean)
  }, ...)
  pgroup <- do.call(rbind, pgroup) 
  pgroup <- aggregate(data.frame(ygroupcorrected = pgroup$x), 
    subset(pgroup, select = c(groupvar, strata)), mean)
  # Calculate ymean corrected for xvars
  px <- lapply(seq_len(nreplications), function(i, ...) {
    tmp <- data
    i <- sample(nrow(data))
    for (var in vars)
      tmp[[var]] <- data[[var]][i]
    p <- predict(model, newdata = tmp, ...)
    aggregate(p, subset(data, select = c(groupvar, strata)), mean)
  }, ...)
  px <- do.call(rbind, px) 
  px <- aggregate(data.frame(yxcorrected = px$x), 
    subset(px, select = c(groupvar, strata)), mean)
  # Calculatre the group average and compate to the overall mean
  tab <- aggregate(data.frame(y = data[[targetvar]]), 
    subset(data, select = c(groupvar, strata)), mean)
  if (!is.null(strata) && length(strata)) {
    ymean <- aggregate(data.frame(ymean = data[[targetvar]]), 
      subset(data, select = c(strata)), mean)
    tab <- merge(tab, ymean, by = strata, all = TRUE)
  } else tab$ymean <- mean(data[[targetvar]])
  tab$diff  <- tab$y - tab$ymean
  # Using the previous predictions: calculate the group effect
  tab <- merge(tab, pgroup, by = c(groupvar, strata), all = TRUE)
  tab <- merge(tab, px, by = c(groupvar, strata), all = TRUE)
  tab$groupeffect <- tab$y - tab$ygroupcorrected
  tab$xeffect     <- tab$y - tab$yxcorrected
  tab$remain      <- tab$y - tab$ymean - tab$groupeffect - tab$xeffect
  tab
}





