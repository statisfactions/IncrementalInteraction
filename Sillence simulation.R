## This file explores the appropriateness of the
## simulation in Sillence et al. (2019),
## ultimately demonstrating that the stated effect
## size in the power analysis is not what would be
## actually expected. This is done through a large
## sample size but a mathematical proof is also
## possible.

# Load packages ------
library(simglm)
library(tidyverse)
library(simpr)

# simpr -------

set.seed(31)
sillence1_spec = specify(x1 =~ rnorm(n),
        x2 =~ rnorm(n),
        x1x2 =~ x1 * x2,
        y =~ b1*x1 + b1 * x2 + b3*x1x2 + rnorm(n)) %>%
  define(n = 1000000,
         b1 = 0.387,
         b3 = 0.15)

sillence_out = sillence1_spec %>%
  generate(1)

sillence_out$sim[[1]] %>% cor

sillence_fit = sillence_out %>%
  fit(lm_full = ~ lm(y ~ x1 * x2),
      lm_reduced = ~ lm(y ~ x1 + x2))

simpr_diff = sillence_fit %>%
  glance_fits %>%
  summarize(diff = abs(diff(r.squared))) %>%
  pull(diff)


# simglm -------
library(broom)

## original main effects
simulation_arguments <- list(
  formula = y ~ 1 + social_anx + facebook_anx + social_anx:facebook_anx,
  fixed = list(social_anx = list(var_type = "continuous"), facebook_anx=list(var_type = "continuous")),
  sample_size = 1000000,
  error = list(variance = 1),
  reg_weights = c(0, 0.387, 0.387, .15)
)

set.seed(12346)
out = simulate_fixed(data = NULL, simulation_arguments) %>%
  simulate_error(simulation_arguments) %>%
  generate_response(simulation_arguments)

simglm_diff = summary(lm(y ~ social_anx * facebook_anx, data = out))$r.squared -
summary(lm(y ~ social_anx + facebook_anx, data = out))$r.squared

## 0 main effects
sim2_args <- list(
  formula = y ~ 1 + social_anx + facebook_anx + social_anx:facebook_anx,
  fixed = list(social_anx = list(var_type = "continuous"), facebook_anx=list(var_type = "continuous")),
  sample_size = 1000000,
  error = list(variance = 1),
  reg_weights = c(0, 0, 0, .15)
)

set.seed(1246)
out2 = simulate_fixed(data = NULL, sim2_args) %>%
  simulate_error(simulation_arguments) %>%
  generate_response(simulation_arguments)

simglm2_diff = summary(lm(y ~ social_anx * facebook_anx, data = out2))$r.squared -
  summary(lm(y ~ social_anx + facebook_anx, data = out2))$r.squared

# image -----
save.image("sillence_sims.Rdata")
