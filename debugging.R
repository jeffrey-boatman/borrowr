# bart testing ----
set.seed(100)
n <- 100
dat <- data.frame(
  src = sample(letters[1:3], n, replace = TRUE),
  trt = sample(c(1, 0), n, replace = TRUE),
  x1  = rnorm(n),
  x2  = rnorm(n),
  y   = rnorm(n)
)

dat$src <- relevel(dat$src, ref = "b")
str(dat)

mf <- model.frame(y ~ src + trt + x1 * x2, dat)
attr(mf, "src_var") <- "src"
attr(mf, "trt_var") <- "trt"
attr(mf, "primary_source") <- "b"
# attr(mf, "in_prim") <- mf[, "src"] == "b"
# debug(fit_mems)
debug(bart)
# undebug(BART::wbart)
foo <- fit_mems(mf, "BART", 10, beta = 2, gf = 16)
bar <- fit_mems(mf, "BART", 10, beta = 2, gf = 64)
fob <- fit_mems(mf, "BART", 10, beta = 2, gf = 4)

# pate testing ----

set.seed(101)
n <- 1000
dat <- data.frame(
  sr = sample(letters[1:3], n, replace = TRUE),
  tr = sample(c(1, 0), n, replace = TRUE),
  x1  = rnorm(n),
  x2  = rnorm(n),
  # x3  = x1,
  y   = rnorm(n)
)
dat$x3 <- dat$x1
# dat$sr <- as.character(dat$sr)
# dat$tr <- as.character(dat$tr)

# debug(pate)
# debug(fit_mems)
# debug(bart)

mf <- pate(y ~ tr + x1,
  estimator      = "bayesian_lm",
  data           = dat,
  src_var        = "sr",
  primary_source = "c",
  trt_var        = "tr",
  ndpost         = 100,
  beta = 1,
  theta = 100)
# debug(bart)
set.seed(123)
bf1 <- pate(y ~ tr,
  estimator      = "BART",
  data           = dat,
  src_var        = "sr",
  primary_source = "c",
  trt_var        = "tr",
  ndpost         = 100,
  beta           = 1,
  theta          = 100)
set.seed(123)
bf2 <- pate(y ~ tr,
  estimator      = "BART",
  data           = dat,
  src_var        = "sr",
  primary_source = "c",
  trt_var        = "tr",
  ndpost         = 100,
  beta           = 1,
  theta          = 100)
head(bf1$pate_post)
head(bf2$pate_post)
all(bf1$pate_post == bf2$pate_post)
bf1$log_marg_like
bf2$log_marg_like

#  mem_fits testing ----

set.seed(101)
n <- 100
dat <- data.frame(
  sr = sample(letters[1:3], n, replace = TRUE),
  tr = sample(c(1, 0), n, replace = TRUE),
  x1  = rnorm(n),
  x2  = rnorm(n),
  y   = rnorm(n)
)
dat$sr <- as.character(dat$sr)

# debug(pate)
# debug(fit_mems)
#debug(bart)
debug(bayes_lm)

debugonce(bart)
bf <- pate(y ~ tr + x1 + x2 + x1:x2,
  estimator      = "BART",
  data           = dat,
  src_var        = "sr",
  primary_source = "c",
  trt_var        = "tr",
  ndpost         = 100,
  beta = 1,
  theta = 100)
debugonce(pate)
debugonce(fit_mems)
debugonce(bayes_lm)
mf <- pate(y ~ tr + x1 + x2 + x1:x2,
  estimator      = "bayesian_lm",
  data           = dat,
  src_var        = "sr",
  primary_source = "c",
  trt_var        = "tr",
  ndpost         = 100,
  beta = 1,
  theta = 100,
  prior_calibration = "empirical")

# use of data(dat)
data(dat)
# debugonce(bayes_lm)
# debug(make_one_prior_tree)
set.seed(93)
bfit <- pate(Y ~ trt + X,
  estimator      = "BART",
  data           = dat,
  src_var        = "src",
  primary_source = "a",
  trt_var        = "trt",
  ndpost         = 100,
  theta          = 100,
  gf             = 16)

# cpp ----
np <- 1e2
out <- make_prior_trees(np, 100, c(2, 4, 6), beta = 1, sigma_mu = rgamma(np, 1, 1))
