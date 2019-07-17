#  borrowr: estimate population average treatment effects with borrowing between data sources.
#  Copyright (C) 2019  Jeffrey A. Verdoliva Boatman
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.

#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.


#'Population Average Treatment Effect (PATE)
#'
#'Estimates the population average treatment effect from a primary data source
#'with potential borrowing from supplemental sources. Adjust for confounding by fitting
#'a conditional mean model with the treatment variable and confounding variables.
#'
#'@details
#'To adjust for confounding, the PATE is estimated using a
#'model for the conditional mean given treatment and confounders. Currently,
#'two models are available, a Bayesian linear model with an inverse-gamma prior,
#'and Bayesian Additive Regression Trees (BART; Chipman & McCulloch, 2010).
#'The user must specify a
#'formula for the conditional mean. This requires more thought for the
#'Bayesian linear model as the analyst must carefully consider the functional
#'form of the regression relationship. For BART, the right hand side of the
#'formula need only include the confounders and the treatment variable without
#'specification of the functional form. If there is no confounding, the right hand
#'side of the formula needs to include the treatment variable only.
#'
#'If \code{formula = "bayesian_lm"}, then the function fits the Bayesian linear model
#'\deqn{Y = X\beta + \epsilon, \epsilon ~ N(0, \sigma ^ 2).}
#'The prior on the regression coefficients is normal with mean vector 0 and variance
#'matrix with diagonal elements equal to 100 and off-diagonal elements equal to 20.
#'The prior on \eqn{\epsilon} is a re-parameterized gamma distribution with a mean of
#'\eqn{\hat{\sigma ^ 2}} and a variance of \eqn{2\hat{sigma ^ 2} / n}, where \eqn{hat{sigma ^ 2}}
#'is the variance from a fitted (frequentist) least squares model.
#'
#'If \code{formula = "BART"}, the function fits the Bayesian Additive Regression Trees
#'model, but with a modified prior on the terminal nodes. The prior on each terminal node
#'is
#'\deqn{N(0, \gamma\sigma ^ 2).} The package uses the default value
#'\deqn{\gamma = 1 / (16 * m * \hat{sigma ^ 2})} where $m$ is the number of trees and \eqn{\hat{sigma ^ 2}} is the variance of \eqn{Y}.
#'
#'Borrowing between data sources is done with
#'Multisource Exchangeability Models
#'(MEMs; Kaizer et al., 2018) . MEMs borrow by assuming that each supplementary data source
#'is either "exchangeable", or not, with the primary data source. Two data sources
#'are considered exchangeable if their model parameters are equal. Each data source
#'can be exchangeable with the primary data, or not, so if there are \eqn{r} data
#'sources, there are \eqn{2 ^ r} possible configurations regarding the exchangeability
#'assumptions. Each of these configurations corresponds to a single MEM.
#'The parameters for each MEM are estimated, and we compute a posterior probability
#'for each. The posterior density of the PATE is a weighted posterior across all
#'possible MEMs.
#'
#'@param formula An object of class formula. The left hand side must be the outcome,
#'and the right hand side must include the treatment variable. To adjust for confounding,
#'the right hand side must also include the confounders. For the Bayesian linear model,
#'the use is responsible for specifying the function form.
#'@param estimator "bayesian_lm" or "BART". If "bayesian_lm", a Bayesian linear model with
#'a normal inverse-gamma prior. If "BART", Bayesian Additive Regression Trees
#'@param data the data frame with the outcome variable and all variables in the formula
#'@param src_var a character variable which variable indicates the source variable.
#'Must match a column name in the data.
#'@param primary_source character variable indicating the primary source. Must match
#'one of the values of \code{src_var}.
#'@param trt_var which variable indicates the treatment.
#'Must match a column name in the data. Must be coded as numeric values 0 and 1, 0 for
#'untreated, 1 for treated.
#'@param ndpost number of draws from the posterior
#'@param ... additional arguments passed to BART
#'
#'@return
#'
#'A list with components:
#'
#'\item{call}{The function call}
#'
#'\item{estimator}{The estimator used to adjust for confounding, "bayesian_lm" for
#'Bayesian linear model, or "BART" for Bayesian additive regression trees}
#'
#'\item{EY0}{Posterior draws of the expected potential outcome if all observations were
#'treated}
#'
#'\item{EY1}{Posterior draws of the expected potential outcome if all observations were
#'untreated}
#'
#'\item{log_marg_like}{Log marginal likelihood for each MEM}
#'
#'\item{mem_pate_post}{Array containing, for each MEM, posterior draws for the population
#'average treatment effect}
#'
#'\item{MEMs}{Matrix showing showing which data sources are exchangeable under each MEM.
#' 1 = exchangeable.}
#'
#'\item{pate_post}{Posterior draws for the population average treatment effect. Weighted
#' average of \code{mem_pate_post}, where \code{post_probs} are the weights}
#'
#'\item{post_probs}{Posterior probability that each MEM (shown in the list element \code{MEMs})
#'is the true model.}
#'
#'
#'@examples
#'data(adapt)
#'
#'est <- pate(y ~ treatment*x + treatment*I(x ^ 2), data = adapt,
#'  estimator = "bayesian_lm", src_var = "source", primary_source = "Primary",
#'  trt_var = "treatment")
#'
#'summary(est)
#'
#'@references
#'Chipman, H. & McCulloch, R. (2010) BART: Bayesian additive regression trees. Annals
#'of Applied Statistics, 4(1): 266-298.
#'
#'Kaizer, Alexander M., Koopmeiners, Joseph S., Hobbs, Brian P. (2018) Bayesian
#' hierarchical modeling based on multisource exchangeability. Biostatistics,
#' 19(2): 169-184.
pate <- function(formula, estimator = c("BART", "bayesian_lm"), data, src_var, primary_source, trt_var,
  ndpost = 1e3, ...) {

  cl <- match.call()

  # error checking
  force(formula)
  force(data)
  estimator <- match.arg(estimator)
  if(!is.character(src_var))
    stop("src_var must be a quoted character variable.")
  if(!is.character(trt_var))
    stop("trt_var must be a quoted character variable.")
  if(is.matrix(data))
    data <- as.data.frame(data)
  if(!(src_var %in% names(data)))
    stop(sprintf("src_var '%s' not found in data.", src_var))
  if(!(trt_var %in% names(data)))
    stop(sprintf("trt_var '%s' not found in data.", trt_var))
  if(is.factor(data[, trt_var]))
    stop(sprintf("trt_var '%s' must be a numeric variable, not a factor.", trt_var))
  # if(!(trt_var %in% all.vars(formula[[3]])))
  #   stop(sprintf("The formula must include the trt_var '%s'.", trt_var))

  # better not do this...
  # nm <- match(c(src_var, trt_var), names(data))
  # names(data)[nm] <- c("src", "trt")


  # remove src_var from formula if present, add it back as with interactions
  # for all variables.
  ot <- terms(formula, data = data) # original formula terms
  fac <- attr(ot, "factors")
  # possible names of source variables in fm:
  pns <- c(src_var, sprintf("as.factor(%s)", src_var))
  if (any(pns %in% rownames(fac))) {
    pns <- pns[which(pns %in% rownames(fac))]
    pns <- match(pns, rownames(fac))
    idx <- which(as.logical(fac[pns, ]))
    formula <- formula(drop.terms(ot, idx, keep.response = TRUE))
  }

  # tns <- c(trt_var, sprintf("as.factor(%s)", trt_var))
  # if (none(tns %in% rownames(fac)))
  #   stop(sprintf("The formula must include the trt_var '%s'.", trt_var))
  tns <- sprintf("as.factor(%s)", trt_var)
  if(tns %in% rownames(fac))
    stop(sprintf("trt_var '%s' must not be a factor in the formula.", trt_var))
  if (trt_var %!in% rownames(fac))
    stop(sprintf("The formula must include the trt_var '%s'.", trt_var))


  # !!! to do !!! ----
  # - add a check to ensure all variables in formula
  # -     are in the data set
  # really dumb way to do it:
  # attr(ot, "factors")
  #  pos <- grep(src_var, attr(terms(formula), "term.labels"))
  #nt <- attr(terms(formula), "term.labels")[-pos]
  # formula <- reformulate(nt, response = all.vars(formula)[[1]])

  # this was code that added source variable an its interactions with
  # all other predictors. no longer doing this.
  # formula <- eval(substitute(update(formula, ~ src_var + .),
  #   list(src_var = as.name(src_var))))

  mf <- data[, c(all.vars(formula), src_var)]
  # verify that trt_var is coded 0-1
  # error is here to ensure that NA values in trt_var have been removed
  treat_vals <- sort(unique(mf[, trt_var]))
  if (!(identical(c(0, 1), treat_vals) | identical(as.integer(c(0, 1)), treat_vals)))
    stop(sprintf("trt_var '%s' must be coded as numeric values 0 and 1", trt_var))

  on <- nrow(mf)
  mf <- na.omit(mf)
  nn <- nrow(mf)
  if(on != nn)
    warning(sprintf("%i rows with missing data removed.", on - nn))

  # mf <- match.call(expand.dots = FALSE)
  # mf$formula <- formula
  # m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
  # mf <- mf[c(1L, m)]
  # mf[[1]] <- quote(stats::model.frame)
  # mf <- eval(mf, parent.frame())
  # src_fac_name <- paste0("as.factor(", src_var, ")")
  # if (!any(match(c(src_var, src_fac_name), names(mf), 0L)))
  # if (!any(c(src_var, src_fac_name) %in% names(mf)))
  #   stop("src_var '", src_var, "' not found in data.")
  #if (!match(src_var, names(mf), 0L))
  srcs <- unique(mf[, src_var])
  ns <- length(srcs)
  dord <- numeric(ns)
  sm <- match(primary_source, srcs, 0L)
  if(!sm)
    stop("primary_source '", primary_source, "' not found in data.")
  if(is.factor(mf[, src_var])) {
    mf[, src_var] <- relevel(mf[, src_var], ref = primary_source)
  } else {
    mf[, src_var] <- factor(mf[, src_var], levels = c(srcs[sm], srcs[-sm]))
  }

  mf[, src_var] <- droplevels(mf[, src_var])

  attr(mf, "formula") <- formula
  attr(mf, "src_var") <- src_var
  attr(mf, "trt_var") <- trt_var
  attr(mf, "primary_source") <- primary_source
  # attr(mf, "in_prim") <- mf[, src_var] == primary_source
  # but what if the formula has specified as.factor for the src variable?
  # and need to make sure that src_var is found in the formula.
  # need to check that the source variance is acharacter, or that the code
  # works even if it's numeric.,
  out <- fit_mems(mf = mf, estimator = estimator, ndpost = ndpost, ...)
  out$call <- cl
  class(out) <- "pate"

  out
}

# set.seed(100)
# n <- 100
# dat <- data.frame(
#   sr = sample(letters[1:3], n, replace = TRUE),
#   tr = sample(c(1, 0), n, replace = TRUE),
#   x1  = rnorm(n),
#   x2  = rnorm(n),
#   y   = rnorm(n)
# )
# dat$sr <- as.character(dat$sr)

# debug(pate)
# debug(fit_mems)
# debug(bart)

# mf <- pate(y ~ as.factor(sr) * (tr * x1 + x2),
#   estimator      = "BART",
#   data           = dat,
#   src_var        = "sr",
#   primary_source = "c",
#   trt_var        = "tr",
#   ndpost         = 100,
#   beta = 1,
#   theta = 100)
# bf <- pate(y ~ as.factor(sr) * (tr * x1 + x2),
#   estimator      = "BART",
#   data           = dat,
#   src_var        = "sr",
#   primary_source = "c",
#   trt_var        = "tr",
#   ndpost         = 100,
#   beta = 2,
#   theta = 100)

