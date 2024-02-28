
N <- 100000
f <- c(0.9, 0.1)
mu <- c(40, 30)
sd <- c(30, 15)


dta <- data.frame(
  groep = sample(1:2, N, prob = f, replace = TRUE)
)
dta$mu <- mu[dta$groep]
dta$sd <- sd[dta$groep]
dta$lft <- rnorm(N, mean = dta$mu, sd = dta$sd)
dta$cgroep <- factor(LETTERS[dta$groep])

library(ggplot2)

ggplot(dta, aes(x = lft, color = cgroep)) + geom_density()


dta$y <- 0.5+ -(dta$groep-1)*0.5 + dta$lft*0.02 + (dta$groep-1)*dta$lft*0.01 + rnorm(1000, sd = 0.2)
dta$y <- 0.5+ -(dta$groep-1)*0.0 + dta$lft*0.02 + (dta$groep-1)*dta$lft*0 + rnorm(1000, sd = 0.2)
ggplot(dta, aes(x = lft, y = y, color = cgroep)) + geom_point()


tab <- aggregate(dta["y"], by = dta["cgroep"], mean)
tab

ggplot(tab, aes(x = cgroep, y = y, fill = cgroep)) + geom_bar(stat = "identity")

m <- lm(y ~ cgroep*lft, data = dta)

tmp <- dta
tmp$lft_orig <- tmp$lft
tmp$cgroep_orig <- tmp$cgroep


tmp$cgroep[] <- "B"
tmp$pred_b <- predict(m, newdata = tmp)
tmp$cgroep[] <- "A"
tmp$pred_a <- predict(m, newdata = tmp)

aggregate(tmp[c("y", "pred_b", "pred_a")], by = tmp["cgroep_orig"], mean)



tmp$cgroep <- tmp$cgroep_orig
tmp$lft[tmp$cgroep == "B"] <- sample(tmp$lft[tmp$cgroep == "A"], sum(tmp$cgroep == "B"))
tmp$pred_lft <- predict(m, newdata = tmp)

tab <- aggregate(tmp[c("y", "pred_b", "pred_a", "pred_lft")], by = tmp["cgroep_orig"], mean)
tab

tab[2, "pred_lft"] - tab[2, "y"]
tab[1, "y"]  - tab[2, "pred_lft"]

tab[2, "pred_a"] - tab[2, "y"]
tab[1, "y"]  - tab[2, "pred_a"]
