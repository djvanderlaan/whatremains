library(data.table)
library(ggplot2)


generatedata <- function(N, pgroup = 0.2, px = c(0.5, 0.8), 
    beta =c(intercept = -2, x = 3, group = -1.0, interaction = 0)) {
  # Generate records; group
  dta <- data.table(
    group = rbinom(N, size = 1, prob = pgroup)
  )
  # Generate x based on group
  dta[, x := rbinom(N, size = 1, prob = px[group+1])]
  # Generate y-variable
  dta[, logitpy := beta["intercept"] + beta["x"]*x + beta["group"]*group + 
    beta["interaction"]*x*group]
  dta[, py := 1/(1 + exp(-logitpy))]
  dta[, y := rbinom(N, size = 1, prob = py)]
}


dta <- generatedata(N = N, pgroup = 0.2, px = c(0.5, 0.8), 
    beta =c(intercept = -2, x = 3, group = -1.0, interaction = 0))
dta[, .(meany = mean(y)), by = .(group)]
dta[, .(meany = mean(y)), by = .(x)]
dta[, .(meany = mean(y)), by = .(group,x)]



source("groupeffect.R")

m <- glm(y ~ factor(group)*factor(x), data = dta, family = binomial())
summary(m)
m2 <- step(m, direction = "backward")

groupeffect(dta, "group", "y", model = m2, type = "response")

groupeffect2(dta, "group", "y", model = m2, type = "response")

pgroup <- seq(0.5, 0.1, length.out = 4)
px <- seq(0.5, 0.9, length.out = 4)
betax <- seq(0, 3, length.out = 4)
betagroup <- seq(-1.0, 1.0, length.out = 5)

params <- CJ(pgroup, px, betax, betagroup) 
params <- split(params, seq_len(nrow(params)))

est <- function(params, N = 10000) {
  dta <- generatedata(N = N, pgroup = params$pgroup, px = c(0.5, params$px), 
      beta =c(intercept = -2, x = params$betax, group = params$betagroup, interaction = 0))
  m <- glm(y ~ factor(group)*factor(x), data = dta, family = binomial())
  m2 <- step(m, direction = "backward", trace = 0)
  res <- groupeffect(dta, "group", "y", model = m2, type = "response", nreplications = 10)
  res$methode <- 1
  res2 <- groupeffect2(dta, "group", "y", model = m2, type = "response", nreplications = 10)
  res2$methode <- 2
  res <- rbind(as.data.table(res), res2, fill = TRUE)
  cbind(res, params)
}

system.time(res <- lapply(params[1:10], est))

res <- lapply(params, est)
res <- rbindlist(res)

library(kitty.r)

kitty_plot({
  g <- ggplot(res[group == 1], aes(x = betagroup, y = groupeffect, color = factor(methode))) + 
    geom_point() + facet_grid(pgroup ~ betax)
  print(g)
})



