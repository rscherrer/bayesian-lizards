rm(list = ls())

library(tidyverse)

theme_set(theme_classic())

set.seed(42)

n <- 100 # number of observations per population
N <- 9 # number of populations

# Parameters
alpha <- 0.01
betaM <- 0.04
betaF <- 0.01
deltaM <- 0.2
deltaF <- 0.04
mu0 <- 0

# For each population...
data <- tibble(

  pop = factor(seq(N)),

  # Sample a strength of sexual selection
  ss = rexp(N),

  # Sample a population mean for each sex based on distance from centroid
  muM = map(ss, ~ rnorm(1, mean = mu0, sd = deltaM * .x)),
  muF = map(ss, ~ rnorm(1, mean = mu0, sd = deltaF * .x)),

  # Sample a population standard deviation based on sexual selection
  sigmaM = alpha + betaM * ss,
  sigmaF = alpha + betaF * ss,

  # Sample individual observations based on mean and standard deviation
  xM = map2(muM, sigmaM, ~ rnorm(n, mean = .x, sd = .y)),
  xF = map2(muF, sigmaF, ~ rnorm(n, mean = .x, sd = .y))

) %>%
  unnest(c(xM, xF)) %>%
  pivot_longer(xM:xF, names_to = "sex", values_to = "x") %>%
  mutate(

    # Post-processing
    sex = str_remove(sex, "x"),
    sigma = if_else(sex == "M", sigmaM, sigmaF)

  )

# Plot
data %>%
  mutate(pop = fct_reorder(pop, sigma)) %>%
  ggplot(aes(x = pop, y = x, fill = sex)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_violin(alpha = 0.5, draw_quantiles = 0.5)
