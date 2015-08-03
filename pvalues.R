library(devtools)
library(data.table)
# install_github("hadley/dplyr@c58a6152406e0130ad4033cb7646de16439b99ea")
library(dplyr)
library(ggplot2)
library(ggthemes)
library(fitdistrplus)

# Source http://www.ctti-clinicaltrials.org/what-we-do/analysis-dissemination/state-clinical-trials/aact-database
d <- fread("results_outcome_analysis.txt")
d <- tbl_df(as.data.frame(d))

# These data are censored. Sometimes authors report p-values
# inexactly ( ~ 21% of all p-values), like < 0.01 or > 0.05.
prop.table(
  xtabs(~ grepl("<", P_VALUE) | grepl(">", P_VALUE),
        data = d)
)
# Truly, if you think about it all p-values are censored because
# there are limits to the precision that computers operate under.
# However, since there's obvious censoring let's treat it as such.
# For example, you'll notice below that right-censored values like,
# " < 0.05" are turned in to a range like: [0, 0.05]. Exact values
# are treated like [0.05, 0.05], and left-censored values are treated
# like [0.05, 1] ( > 0.05).

d <- d %>%
  mutate(# simple case, p-values are numeric
    left = as.numeric(P_VALUE),
    right = as.numeric(P_VALUE),
    # if a p-value looks like < 0.01, its left-bound is 0
    left = ifelse(grepl("<", P_VALUE),
                  0, left),
    # if a p-value looks like > 0.01, its right-bound is 1
    right = ifelse(grepl(">", P_VALUE),
                   1, right),
    # if a p-value looks like < 0.01, its right-bound is 0.01
    right = ifelse(grepl("<", P_VALUE),
                   as.numeric(
                     gsub("<", "", gsub("<=", "", P_VALUE))),
                   right),
    # if a p-value looks like > 0.01, its left-bound is 0.01
    left = ifelse(grepl(">", P_VALUE),
                  as.numeric(
                    gsub(">", "", gsub(">=", "", P_VALUE))),
                  left)) %>%
  mutate(
    # if a p-value looks like '= 0.01', its left-bound is 0.01
    left = ifelse(grepl("=", P_VALUE) & !grepl("<", P_VALUE) & !grepl(">", P_VALUE),
                  as.numeric(
                    gsub("=", "", P_VALUE)
                  ), left),
    # if a p-value looks like '= 0.01', its right-bound is 0.01
    right = ifelse(grepl("=", P_VALUE) & !grepl("<", P_VALUE) & !grepl(">", P_VALUE),
                   as.numeric(
                     gsub("=", "", P_VALUE)
                   ), right)) %>%
  # there are a handful of values that fall outside of values that actually
  # fall outside the allowed range of p-values (probabilities must range
  # from 0 to 1). ** I'll simply throw those away. **
  filter(left >= 0 & left <= 1,
         right >= 0 & right <= 1)

d <- as.data.frame(d)
fit <- fitdistcens(
  d[, c("left", "right")],
  "beta",
  start = list(shape1 = 1, shape2 = 3))
b1 <- bootdistcens(fit, niter = 10)
plot(b1)

d %>%
  filter(left == right) %>%
  select(left)

ggplot(
  data.frame(
  x = rbeta(length(d$left),
      shape1 = median(b1$estim$shape1),
      shape2= median(b1$estim$shape2))),
  aes(x = x)) +
  geom_histogram(binwidth = 0.001)

plot(fit, leftNA = 0, rightNA = 0)

d %>%
  select(left, P_VALUE, right)

ggplot(d[d$left == d$right, ],
       aes(x = left)) +
  geom_histogram(binwidth = 0.001,
                 colour = "red") +
  geom_vline(xintercept = 0.05) +
  scale_x_continuous(breaks = seq(0, 1, 0.01)) +
  coord_cartesian(xlim = c(0, 0.5)) +
  ggtitle("All ClinicalTrials.gov p-values") +
  theme_bw(base_size = 25)




