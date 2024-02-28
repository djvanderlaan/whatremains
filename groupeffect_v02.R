groupeffect <- function(data, groupvar, targetvar, model, nreplications = 100) {
  # Assign each member in the dataset a random group sampled from teh distribution
  # of groups in the dataset. The goal is to make each record member of the average
  # group. We then calculate predictions using the model. We repeat a number of
  # times to average out the sampling
  p <- replicate(nreplications, {
    tmp <- data
    tmp[[groupvar]] <- sample(data[[groupvar]])
    p <- predict(model, newdata = tmp)
    aggregate(p, data[, groupvar, drop = FALSE], mean)
  }, simplify = FALSE)
  p <- do.call(rbind, p) 
  p <- aggregate(p[[2]], p[1], mean)
  names(p)[2] <- "ygroupcorrected"
  # Calculatre the group average and compate to the overall mean
  tab <- aggregate(data.frame(y = data[[targetvar]]), dta[, groupvar, drop = FALSE], mean)
  tab$ymean <- mean(data[[targetvar]])
  tab$diff  <- tab$y - tab$ymean
  # Using the previous predictions: calculate the group effect
  tab <- merge(tab, p, by = groupvar, all = TRUE)
  tab$groupeffect <- tab$y - tab$ygroupcorrected
  tab$remain      <- tab$ygroupcorrected - tab$ymean
  tab
}

groupeffect2 <- function(data, groupvar, targetvar, model, nreplications = 100) {
  vars <- formula(model) |> all.vars() |> setdiff(c(groupvar, targetvar))
  # Assign each member in the dataset a random group sampled from teh distribution
  # of groups in the dataset. The goal is to make each record member of the average
  # group. We then calculate predictions using the model. We repeat a number of
  # times to average out the sampling
  p <- replicate(nreplications, {
    tmp <- data
    i <- sample(nrow(data))
    tmp[vars] <- data[i, vars, drop = FALSE]
    p <- predict(model, newdata = tmp)
    aggregate(p, data[groupvar], mean)
  }, simplify = FALSE)
  p <- do.call(rbind, p) 
  p <- aggregate(p[[2]], p[1], mean)
  names(p)[2] <- "yxcorrected"
  # Calculatre the group average and compate to the overall mean
  tab <- aggregate(data.frame(y = data[[targetvar]]), dta[groupvar], mean)
  tab$ymean <- mean(data[[targetvar]])
  tab$diff  <- tab$y - tab$ymean
  # Using the previous predictions: calculate the group effect
  tab <- merge(tab, p, by = groupvar, all = TRUE)
  tab$xeffect     <- tab$y - tab$yxcorrected
  tab$groupeffect <- tab$yxcorrected - tab$ymean
  tab
}
