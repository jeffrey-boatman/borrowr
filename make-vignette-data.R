set.seed(322)

n <- 60
ns <- 3
N <- ns * n

A <- rbinom(N, 1, 0.5)
s <- c("Primary", "Supp1", "Supp2")
S <- sample(s, N, replace = TRUE)
o <- c(0, -1, 1)[match(S, s)]

X <- rnorm(N, 30 + 20 * A, sd = 10)

Y0 <- rnorm(N, 72 + 3 * sqrt(X) + o * 2, 2)
Y1 <- rnorm(N, 90 + exp(0.04 * X) - o * 2, 2)
Y <- A * Y1 + (1 - A) * Y0
C <- rbinom(n, 1, plogis(0.05 * X))

adapt <- data.frame(
  y = Y,
  x = X,
  source = S,
  # treatment = factor(A)
  treatment = A,
  compliant = C
)

adapt <- adapt[order(adapt$source), ]
rownames(adapt) <- NULL

library(ggplot2)
ggplot(data = adapt, mapping = aes(x = x, y = y, color = as.factor(treatment))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ source) +
  theme_classic()

save("adapt", file = "./data/adapt.RData")
