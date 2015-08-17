library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(fitdistrplus)
# image_output_path <- "~/statwonk/content/images/"

# Source http://www.ctti-clinicaltrials.org/what-we-do/analysis-dissemination/state-clinical-trials/aact-database
d <- fread("data/results_outcome_analysis.txt")

# These data are censored. Sometimes authors report p-values
# inexactly ( ~ 21% of all p-values), like < 0.01 or > 0.05.
percent(
  prop.table(
    xtabs(~ grepl("<", P_VALUE) | grepl(">", P_VALUE),
          data = d)
  )[2]
)# It's not surprising to me that ~ 1 in 5 p-values
# are censored, but it complicates analyzing the data.
# Censorship is an important aspect that needs to be
# respected when analyzing data. Bad things happen
# when you don't respect censoring.
#
# Further reading on censoring:
# https://books.google.com/books?id=Q01YBAAAQBAJ&lpg=PT436&dq=Statistical%20Methods%20for%20Reliability%20Data&pg=PP1#v=onepage&q=Statistical%20Methods%20for%20Reliability%20Data&f=false
# https://www.youtube.com/watch?v=XQfxndJH4UA
table(d$P_VALUE[grepl("<", d$P_VALUE)])

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
  # from 0 to 1).
  # ** I'll simply throw those away. **
  # Have a better idea? Drop me a line.
  # Also, notice that I throw away values of
  # 0 and 1. We're about to fit a Beta
  # distribution, which does not have support
  # inclusive of 0 or 1. I'm guessing this is
  # part of the reason that fitdistcens
  # won't fit a vector with those values.
filter(left > 0 & left < 1,
       right > 0 & right < 1)

d <- as.data.frame(d)

# I highly recommend the fitdistplus
# package. It's incredibly useful for
# parametric fitting.
fit <- fitdistcens(
  d[, c("left", "right")],
  "beta",
  start = list(shape1 = 1, shape2 = 3))

# A look at the fit.
fit

# Not a bad fit! However, p-values don't follow a
# theoretical distribution. Under the null, p-values
# are uniform(0, 1) distributed, but no such theory
# underpins the alternative. The final product,
# published p-values are a mixture of a uniform(0, 1)
# distribution and some other distribution that represents
# how successful scientists are at leveraging their prior
# knowledge / information for designing and carrying out
# experiments.
plot(fit)
# Also, notice that there aren't any major discontinuities
# visible here. The fit is nice, but it's not perfect ---
# probably because theory can't yet underpin this mixture
# well. Tell me if I'm wrong! I don't think I am.

parametric_comparison_plot <- ggplot() +
  xlab("Raw numeric p-values") +
  geom_histogram(
    data = d,
    aes(x = left),
    binwidth = 0.001,
    fill = 'orange') +
  geom_histogram(
    data = data.frame(
      x = rbeta(length(d$left),
                shape1 = round(fit$estimate[[1]], 2),
                shape2=  round(fit$estimate[[2]]))),
    aes(x = x),
    binwidth = 0.001) +
  scale_y_continuous(breaks = seq(0, length(d$P_VALUE), 250),
                     labels = comma) +
  ggtitle(
    paste0("Empirical p-values vs. predicted by theoretical Beta(",
           "alpha=", fit$estimate[[1]], ",",
           "beta=", fit$estimate[[2]])) +
  theme_bw(base_size = 25)

# This parametric fit is interesting to
# look at for data nerds, but it's
# fruit from a poison branch --- one
# half of the p-value mixture has some
# unknown distribution. For example, is
# it unreasonable to round your p-value like
# 0.01, 0.02, 0.03 like below? I don't think so.
# Approximation is an important aspect of doing
# good data analysis and communication.
parametric_comparison_plot

# Again, _interesting_. Tempting to point to
parametric_comparison_plot +
  coord_cartesian(xlim = c(0, 0.1)) +
  annotate(
    geom = "text", x = 0.04, y = 500,
    label = "This difference could just be well-informed hypotheses.")

