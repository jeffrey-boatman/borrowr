## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(combr)
data(adapt)
head(adapt)

## ---- fig.height = 3, fig.width = 8--------------------------------------
library(ggplot2)
ggplot(data = adapt, mapping = aes(x = x, y = y, color = as.factor(treatment))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ source) +
  theme_classic()

## ------------------------------------------------------------------------
est <- pate(y ~ treatment*x + treatment*I(x ^ 2), data = adapt, 
  estimator = "bayesian_lm", src_var = "source", primary_source = "Primary", 
  trt_var = "treatment")

## ------------------------------------------------------------------------
est

## ------------------------------------------------------------------------
summary(est)

