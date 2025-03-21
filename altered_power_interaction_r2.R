function (N, r.x1.y, r.x2.y, r.x1x2.y, r.x1.x2, rel.x1 = 1, rel.x2 = 1,
          rel.y = 1, alpha = 0.05, detailed_results = FALSE)
{
  settings <- expand.grid(list(N = N, r.x1.y = r.x1.y, r.x2.y = r.x2.y,
                               r.x1.x2 = r.x1.x2, r.x1x2.y = r.x1x2.y, rel.x1 = rel.x1,
                               rel.x2 = rel.x2, rel.y = rel.y, alpha = alpha))
  settings$N <- round(settings$N)
  if (min(c(settings$rel.x1, settings$rel.x2, settings$rel.y)) <=
      0 | max(c(settings$rel.x1, settings$rel.x2, settings$rel.y)) >
      1) {
    stop("All reliabilities must be greater than 0 and less than or equal to 1")
  }
  if (max(abs(c(settings$r.x1.y, settings$r.x2.y, settings$r.x1.x2,
                settings$r.x1x2.y))) > 1) {
    stop("All correlations must be within [-1,1]")
  }
  settings$rel.x1x2 = NA
  for (i in 1:dim(settings)[1]) {
    settings$rel.x1x2[i] = ((settings$rel.x1[i] * settings$rel.x2[i]) +
                              (settings$r.x1.x2[i]^2))/(1 + (settings$r.x1.x2[i]^2))
    settings$obs.r.x1.y[i] = settings$r.x1.y[i] * sqrt(settings$rel.x1[i] *
                                                         settings$rel.y[i])
    settings$obs.r.x2.y[i] = settings$r.x2.y[i] * sqrt(settings$rel.x2[i] *
                                                         settings$rel.y[i])
    settings$obs.r.x1.x2[i] = settings$r.x1.x2[i] * sqrt(settings$rel.x1[i] *
                                                           settings$rel.x2[i])
    settings$obs.r.x1x2.y[i] = settings$r.x1x2.y[i] * sqrt(settings$rel.x1x2[i] *
                                                             settings$rel.y[i])
  }
  set2 <- unique(settings[, c(11:14)])
  omit <- NA
  i <- NULL
  for (i in 1:dim(set2)[1]) {
    out <- base::tryCatch(expr = {
      data <- generate_interaction(N = 10, adjust.correlations = F,
                                   r.x1.y = set2$obs.r.x1.y[i], r.x2.y = set2$obs.r.x2.y[i],
                                   r.x1x2.y = set2$obs.r.x1x2.y[i], r.x1.x2 = set2$obs.r.x1.x2[i])
      data <- 0
    }, warning = function(cond) {
      i
    }, error = function(cond) {
      i
    })
    omit <- c(omit, out)
  }
  omit <- stats::na.omit(omit)
  bad_ones <- omit[omit > 0]
  if (length(bad_ones) > 0) {
    to_be_removed <- set2[bad_ones, ]
    to_be_removed$Removed = 1
    settings2 <- merge(settings, to_be_removed, all.x = T)
    removed <- settings2[!is.na(settings2$Removed), ]
    warning(paste(round(dim(removed)[1]/dim(settings)[1] *
                          100, 2), " % of requested simulations are impossible, n=",
                  dim(removed)[1], ". Removing from list.", sep = ""))
    warning(to_be_removed)
    settings <- settings2[is.na(settings2$Removed), -14]
    if (dim(settings)[1] == 0) {
      stop("No valid settings")
    }
  }
  analytic_pwr = function(N, r.x1.y, r.x2.y, r.x1x2.y, r.x1.x2,
                          alpha) {
    sd.x1 = 1
    sd.x2 = 1
    sd.y = 1
    mean.x1 = 0
    mean.x2 = 0
    mean.y = 0
    r.x1.x1x2 = 0
    r.x2.x1x2 = 0
    cor.mat = matrix(data = c(1, r.x1.x2, r.x1.x2, 1), nrow = 2,
                     byrow = T)
    sd = c(sd.x1, sd.x2)
    cov_x1x2 <- diag(sd) %*% cor.mat %*% diag(sd)
    sd.x1x2 = base::sqrt((cov_x1x2[1, 1] * cov_x1x2[2, 2]) +
                           ((cov_x1x2[1, 2])^2))
    cormat <- base::matrix(data = c(1, r.x1.x2, r.x1.x1x2,
                                    r.x1.y, r.x1.x2, 1, r.x2.x1x2, r.x2.y, r.x1.x1x2,
                                    r.x2.x1x2, 1, r.x1x2.y, r.x1.y, r.x2.y, r.x1x2.y,
                                    1), ncol = 4, byrow = TRUE)
    sd = c(sd.x1, sd.x2, sd.x1x2, sd.y)
    covmat <- diag(sd) %*% cormat %*% diag(sd)
    b <- covmat[4, -4]
    A <- covmat[-4, -4]
    betas <- base::solve(A, b)
    b1 <- betas[1]
    b2 <- betas[2]
    b3 <- betas[3]
    b1a = b1
    b2a = b2
    totalr2 = (b1 * r.x1.y) + (b2 * r.x2.y) + (b3 * r.x1x2.y)
    cormat <- base::matrix(data = c(1, r.x1.x2, r.x1.y, r.x1.x2,
                                    1, r.x2.y, r.x1.y, r.x2.y, 1), ncol = 3, byrow = TRUE)
    sd = c(sd.x1, sd.x2, sd.y)
    covmat <- diag(sd) %*% cormat %*% diag(sd)
    b <- covmat[3, -3]
    A <- covmat[-3, -3]
    betas <- base::solve(A, b)
    b1 <- betas[1]
    b2 <- betas[2]
    null2 = (b1 * r.x1.y) + (b2 * r.x2.y)
    b1 = b1a
    b2 = b2a
    df_denom <- (N - 2) - 1
    f2 <- (totalr2 - null2)/(1 - null2)
    lambda = f2 * df_denom
    minusalpha <- 1 - alpha
    Ft <- stats::qf(minusalpha, 1, df_denom)
    Power <- 1 - stats::pf(Ft, 1, df_denom, lambda)
    return(c(Power, b1, b2, b3))
  }
  settings$pwr = NA
  for (i in 1:dim(settings)[1]) {
    power_result = analytic_pwr(N = settings$N[i], r.x1.y = settings$obs.r.x1.y[i],
                                r.x2.y = settings$obs.r.x2.y[i], r.x1x2.y = settings$obs.r.x1x2.y[i],
                                r.x1.x2 = settings$obs.r.x1.x2[i], alpha = settings$alpha[i])
    settings$pwr[i] = power_result[1]
    settings$b1[i] = power_result[2]
    settings$b2[i] = power_result[3]
    settings$b3[i] = power_result[4]
  }
  settings = cbind(settings$pwr, settings[, c(1:14, 16:18)])
  colnames(settings)[1] = "pwr"
  settings$shape = settings$obs.r.x1x2.y/settings$obs.r.x1.y
  if (detailed_results == F) {
    settings = settings[, -c(11:19)]
    dimnum <- sapply(X = c(1:dim(settings)[2]), FUN = function(x) {
      length(table(settings[, x]))
    })
    grouping_variables <- colnames(settings)[dimnum > 1]
    if (length(grouping_variables) > 1) {
      grouping_variables = c(grouping_variables[-1], grouping_variables[1])
      settings = settings[, match(x = grouping_variables,
                                  colnames(settings))]
    }
    else {
      settings = data.frame(pwr = settings$pwr)
    }
  }
  else {
    dimnum <- sapply(X = c(1:dim(settings)[2]), FUN = function(x) {
      length(table(settings[, x]))
    })
    dimnum[c(11:19)] = 1
    grouping_variables <- colnames(settings)[dimnum > 1]
    if (length(grouping_variables) > 1) {
      grouping_variables = c(grouping_variables[-1], grouping_variables[1])
      settingsa = settings[, match(x = grouping_variables,
                                   colnames(settings))]
      settingsb = settings[, -match(x = grouping_variables,
                                    colnames(settings))]
      settings = cbind(settingsa, settingsb)
    }
  }
  return(settings)
}
