library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(fitdistrplus)

# Source http://www.ctti-clinicaltrials.org/what-we-do/analysis-dissemination/state-clinical-trials/aact-database
d <- fread("results_outcome_analysis.txt")

# These data are censored. Sometimes authors report p-values
# inexactly ( ~ 21% of all p-values), like < 0.01 or > 0.05.
prop.table(
  xtabs(~ grepl("<", P_VALUE) | grepl(">", P_VALUE),
        data = d)
) # It's not surprising to me that ~ 1 in 5 p-values
# are censored, but it complicates analyzing the data.
# Censorship is an important aspect that needs to be
# respected when analyzing data. Bad things happen
# when you don't respect censoring.
table(d$P_VALUE[grepl("<", d$P_VALUE)])

ggplot(tbl_df(d[grepl("<0.", P_VALUE)]) %>%
         group_by(P_VALUE) %>%
         summarise(count = n()),
       aes(x = P_VALUE,
           y = count)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, length(d$P_VALUE), 500)) +
  ylab("Count") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(
    size = 10,
    angle = 45,
    hjust = 1,
    vjust = 1),
    panel.grid.minor = element_blank()) +
  annotate(geom = "text", x = "<0.05", y = 750,
           label = "500 Sloppily reported p-values")

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

tryCatch({
  fitdistcens(
    as.data.frame( # fitdistcense requires data.frame object
      d[, c("left", "right")]),
    "beta",
    start = list(shape1 = 1,
                 shape2 = 3),
    gr = "BFGS", # tried working with optim to see
    method = "BFGS", # if I could get it to optimize
    lower = 0, # no luck.
    upper = 1)
},
finally = print(
  "Errors because apparently
   fitdistcens (or optim) can't
   handle beta values of 0 or 1??"))

d <- d %>%
  # throwing away values of 0 or 1.
  filter(left > 0 & left < 1,
         right > 0 & right < 1)

censfit <- fitdistcens(
  as.data.frame( # fitdistcense requires data.frame object
    d[, c("left", "right")]),
  "beta",
  start = list(shape1 = 1,
               shape2 = 3))

d <- d %>%
  filter(left == right) %>%
  select(left)

fit <- fitdist(data = d$left, "beta")
ppcomp(fit,
       xlim = c(0, 0.06),
       ylim = c(0, 0.06))
ppcomp(fit)

b_fit <- bootdist(fit,
                  niter = 10)
b_cens <- bootdistcens(censfit,
                       niter = 10)

ggplot() +
  geom_density(
    data = data.frame(
      x = rbeta(length(d$left),
                shape1 = median(b_fit$estim$shape1),
                shape2= median(b_fit$estim$shape2))),
    aes(x = x),
    colour = "blue") +
  geom_density(
    data = data.frame(
      x = rbeta(length(d$left),
                shape1 = median(b_cens$estim$shape1),
                shape2= median(b_cens$estim$shape2))),
    aes(x = x),
    colour = "green") +
  geom_density(
    data = data.frame(
      x = d$left),
    aes(x = x),
    colour = "red")

