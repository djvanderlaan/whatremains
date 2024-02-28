library(ggplot2)
library(kitty.r)

generatedata <- function(N = 10000, groupeffect = 1, disteffect = 1) {
  groepen <- LETTERS[1:2]
  probs   <- c(0.9, 0.1)
  probs   <- probs/sum(probs)
  mean    <- c(45, 45 - disteffect*(45-25))
  sd      <- c(20, 20 - disteffect*(20-10))
  pman    <- c(0.5, 0.5 - disteffect*(0.5-0.7))
  if (!disteffect) {
    # Both groups have same distribution
    mean[2] <- mean[1]
    sd[2]   <- sd[1]
    pman[2] <- pman[1]
  }
  dta <- data.frame(
    groepid = sample(length(groepen), N, replace = TRUE, prob = probs)
    )
  dta$groep <- groepen[dta$groepid]
  dta$x1    <- rnorm(N, mean = mean[dta$groepid], sd = sd[dta$groepid])
  dta$x2    <- rbinom(N, size = 1, prob = pman[dta$groepid])
  # Generate y-variable
  dta$groepB <-(dta$groep == "B")*1
  #if (groupeffect) {
    dta$y <- -0.5 - dta$x1 * 0.01 + groupeffect*dta$groepB*0.2 + dta$x2*0.3 + 
      -dta$x2*dta$x1*0.002 - groupeffect*dta$groepB*dta$x1*0.003 + rnorm(N, sd = 0.1)
  #} else {
    #dta$y <- -0.5 - dta$x1 * 0.01 + dta$groepB*0.2*0 + dta$x2*0.3 + 
      #-dta$x2*dta$x1*0.002 - dta$groepB*dta$x1*0.003*0 + rnorm(N, sd = 0.1)
  #}
  dta$groepB <- NULL
  dta
}

groupeffect <- function(data, groupvar, targetvar, model, nreplications = 100) {
  # Assign each member in the dataset a random group sampled from teh distribution
  # of groups in the dataset. The goal is to make each record member of the average
  # group. We then calculate predictions using the model. We repeat a number of
  # times to average out the sampling
  p <- replicate(nreplications, {
    tmp <- data
    tmp[[groupvar]] <- sample(data[[groupvar]])
    p <- predict(model, newdata = tmp)
    aggregate(p, data[groupvar], mean)
  }, simplify = FALSE)
  p <- do.call(rbind, p) 
  p <- aggregate(p[[2]], p[1], mean)
  names(p)[2] <- "ygroupcorrected"
  # Calculatre the group average and compate to the overall mean
  tab <- aggregate(data.frame(y = data[[targetvar]]), dta[groupvar], mean)
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


dta <- generatedata(groupeffect = 2, disteffect = 2)

kitty_plot({
  g <- ggplot(dta, aes(x = x1, color = groep)) + geom_density()
  print(g)
})
kitty_plot({
  g <- ggplot(dta, aes(x = groep, fill = factor(x2))) + geom_bar()
  print(g)
})
kitty_plot({
  g <- ggplot(dta, aes(x = x1, color = groep, y = y)) + geom_point(alpha = 0.1) +
    geom_smooth() 
  print(g)
})

m0 <- lm(y ~ x1*x2, data = dta)
m <- lm(y ~ x1*x2*groep, data = dta)
anova(m0, m, test = "Chisq")

res <- groupeffect(dta, "groep", "y", model = m)
res

#res$groupeffect / (abs(res$groupeffect) + abs(res$remain)) * 100
#res$remain / (abs(res$groupeffect) + abs(res$remain)) * 100

res2 <- groupeffect2(dta, "groep", "y", model = m)
res2






groupeffect <- 2
disteffect <- 1

dta <- generatedata(groupeffect = 2, disteffect = 2)
m <- lm(y ~ x1*x2*groep, data = dta)
res <- groupeffect(dta, "groep", "y", model = m)
res$groupeffect <- groupeffect
res$disteffect <- disteffect
res$methode <- 1


#res$groupeffect / (abs(res$groupeffect) + abs(res$remain)) * 100
#res$remain / (abs(res$groupeffect) + abs(res$remain)) * 100

res2 <- groupeffect2(dta, "groep", "y", model = m)
res2$groupeffect <- groupeffect
res2$disteffect <- disteffect
res2$methode <- 1

rbind(res, res2)



