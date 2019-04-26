#  combr: combine data sources to estimate population average treatment effect.
#  Copyright (C) <2019>  <Jeffrey A. Verdoliva Boatman>
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


#'Printing Population Average Treatment Effects Object
#'
#'\code{print} method for class "\code{pate}".
#'
#'@param x an object of class "\code{pate}"
#'@param ... further arguments passed to or from other methods.
print.pate <- function(x, ...) {
  cat("Population Average Treatment Effect (PATE)\n")
  cat("\nPATE Posterior Summary Statistics (Treated vs. Untreated)\n\n")
  # posterior statistics
  post <- x$pate_post
  psum <- posterior_summary(post)
  print(psum)
  out <- list(posterior_summary = psum)
  cat("\n")
  invisible(out)
}

#'Summary of Population Average Treatment Effect
#'
#'\code{summary} for class "\code{pate}".
#'
#'@param object an object of class "\code{pate}".
#'@param ... further arguments passed to or from other methods/
summary.pate <- function(object, ...) {
  x <- object
  cat("\nPopulation Average Treatment Effect (PATE)\n")
  cat("\nCall:\n\n")
  print(x$call)
  cat("\nPATE Posterior Summary Statistics (Treated vs. Untreated)\n\n")
  # posterior statistics
  post <- x$pate_post
  psum <- posterior_summary(post)
  print(psum)
  cat("\nExchangeability Matrix (1 == Exchangeable with primary source):\n\n")
  print(x$mems + 0)
  cat("\nMEM Posterior Probability:\n")
  print(x$post_probs)
  cat("\n")
  out <- list()
  out$call <- x$call
  out$posterior_summary <- psum
  out$mems <- x$mems + 0
  out$post_probs <- x$post_probs
  invisible(out)
}
