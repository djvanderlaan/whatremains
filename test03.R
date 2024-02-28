library(data.table)
library(ggplot2)

generatedata <- function(table, N) {
  table <- read.table(textConnection(table), header = TRUE)
  table$prob <- sapply(table$prob, \(x) eval(parse(text = x)))
  table$size    <- N/sum(table$subsize)*table$size
  table$subsize <- N/sum(table$subsize)*table$subsize
  dta <- table[ rep(seq_len(nrow(table)), table$subsize), ]
  dta$y <- rbinom(nrow(dta), size = 1, prob = dta$prob)
  setDT(dta)
  dta
}

source("groupeffect2.R")

# Example 1:
# There is both an effect of x and group
table <- "
group size sub subsize prob
A      800  a   400     0.1+0.4
A      800  b   400     0.1+0.0
B      200  a   180     0.1+0.4-0.1
B      200  b   20      0.1+0.0-0.1 "

dta <- generatedata(table, 10000)

dta[, .(n = .N, ny = sum(y), y = mean(y)), by = .(group)] |> 
  transform(ymean = sum(ny)/sum(n)) |> 
  transform(diff = y - ymean)

m <- glm(y ~ factor(group)*factor(sub), data = dta, family = binomial())
groupeffect(dta, "group", "y", model = m, type = "response")
groupeffect2(dta, "group", "y", model = m, type = "response")



# Test if the estimate using groupeffect gives the same result as using the 
# average group in the prediction. groupeffect randomly samples from the 
# groups. In case of no linear effects and linear regression this should give
# the same results
dta[, group2 := (group == "B")*1]
m <- glm(y ~ group2*factor(sub), data = dta, family = gaussian())
groupeffect(dta, "group2", "y", model = m, type = "response")
groupeffect2(dta, "group2", "y", model = m, type = "response")
# In case of linear regression without interactions the following is identical
# to the estimate of groupeffect. Here we take the average group and calculator
# predictions. 
tmp <- copy(dta)
tmp[, group2_orig := group2]
tmp[, group2 := mean(group2)]
tmp[, p := predict(m, newdata = tmp, type = "response")]
tmp[, .(mean(p)), by = group2_orig]



# Example 2: there is no groupeffect
table <- "
group size sub subsize prob
A      800  a   400     0.1+0.4
A      800  b   400     0.1+0.0
B      200  a   180     0.1+0.4
B      200  b   20      0.1+0.0"

dta <- generatedata(table, 10000)

dta[, .(n = .N, ny = sum(y), y = mean(y)), by = .(group)] |> 
  transform(ymean = sum(ny)/sum(n)) |> 
  transform(diff = y - ymean)

m <- glm(y ~ factor(group)*factor(sub), data = dta, family = binomial())
groupeffect(dta, "group", "y", model = m, type = "response")
groupeffect2(dta, "group", "y", model = m, type = "response")

# Test if the estimate using groupeffect gives the same result as using the 
# average group in the prediction. groupeffect randomly samples from the 
# groups. In case of no linear effects and linear regression this should give
# the same results
dta[, group2 := (group == "B")*1]
m <- glm(y ~ group2*factor(sub), data = dta, family = gaussian())
groupeffect(dta, "group2", "y", model = m, type = "response")
# In case of linear regression without interactions the following is identical
# to the estimate of groupeffect. Here we take the average group and calculator
# predictions. 
tmp <- copy(dta)
tmp[, group2_orig := group2]
tmp[, group2 := mean(group2)]
tmp[, p := predict(m, newdata = tmp, type = "response")]
tmp[, .(mean(p)), by = group2_orig]






# Example 2: there is and interaction between groupeffect and effect of x
table <- "
group size sub subsize prob
A      800  a   400     0.1+0.4
A      800  b   400     0.1+0.0
B      200  a   180     0.1+0.4+0.1
B      200  b   20      0.1+0.0-0.1"

dta <- generatedata(table, 10000)

dta[, .(n = .N, ny = sum(y), y = mean(y)), by = .(group)] |> 
  transform(ymean = sum(ny)/sum(n)) |> 
  transform(diff = y - ymean)

m <- glm(y ~ factor(group)*factor(sub), data = dta, family = binomial())
groupeffect(dta, "group", "y", model = m, type = "response")
groupeffect2(dta, "group", "y", model = m, type = "response")


# Test if the estimate using groupeffect gives the same result as using the 
# average group in the prediction. groupeffect randomly samples from the 
# groups. In case of no linear effects and linear regression this should give
# the same results
dta[, group2 := (group == "B")*1]
m <- glm(y ~ group2*factor(sub), data = dta, family = gaussian())
groupeffect(dta, "group2", "y", model = m, type = "response")
# In case of linear regression without interactions the following is identical
# to the estimate of groupeffect. Here we take the average group and calculator
# predictions. 
tmp <- copy(dta)
tmp[, group2_orig := group2]
tmp[, group2 := mean(group2)]
tmp[, p := predict(m, newdata = tmp, type = "response")]
tmp[, .(mean(p)), by = group2_orig]
# In this example this still seems to hold in case there are interactions :think: 






groupeffect(dta, "group", "y", model = m, type = "response", strata = "sub")
groupeffect2(dta, "group", "y", model = m, type = "response", strata = "sub")

