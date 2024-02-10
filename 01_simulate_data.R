rm(list = ls())

library(tidyverse)

# Parameters
n <- 9
sigma0 <- 0.01
d0 <- 0
alphaM <- 0.05
alphaF <- 0.01
betaM <- 0.05
betaF <- 0.01
x0 <- 0
N <- 100

# For each population...
pops <- tibble(

  # Sample a value for the intensity of sexual selection
  j = seq(n),
  s = rexp(n)

) %>%
  expand_grid(

    # Add both sexes
    sex = c(TRUE, FALSE)

  ) %>%
  mutate(

    # Compute sex-specific variance, distance to optimum and mean phenotype
    sigma = sigma0 + s * if_else(sex, alphaM, alphaF),
    d = d0 + s * if_else(sex, betaM, betaF),
    mu = map_dbl(d, \(d) rnorm(1, x0, d))

  )

# Simulate individuals
data <- pops %>%
  mutate(x = map2(mu, sigma, \(mu, sigma) rnorm(N, mu, sigma))) %>%
  unnest(x)

data
