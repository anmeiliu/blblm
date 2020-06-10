#' @import purrr
#' @import stats
#' @import utils
#' @import future
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

# TODO: allow GLM?

#' @export
blblm <- function(formula, family = gaussian, data = NULL, filepaths = NULL, read_function = read.csv, m = 10, B = 5000, use_plan = TRUE, ...) {
  if (is.null(data) & is.null(filepaths)) {
    stop("Neither data nor filepaths to data provided")
  }
  if (!is.null(data) & !is.null(filepaths)) {
    warning("Both data and filepaths specified, using data")
  }
  if (!is.null(filepaths) & length(filepaths) != m) {
    warning("Number of filepaths provided is not the same as number of splits, using file-based splits")
  }
  if (use_plan & grepl("sequential", deparse(attributes(plan())$call))) {
    warning("Using a sequential plan (this is usually slower than not using a plan)")
  }

  if (use_plan) {
    active_map <- furrr::future_map
  } else {
    active_map <- map
  }

  if (!is.null(data)) {
    data_list <- split_sample(data, m)
    estimates <- active_map(data_list, ~ lm_each_subsample(formula = formula, family = family, data = ., n = nrow(data), B = B))
  } else {
    estimates <- active_map(filepaths, function(filepath_split) {
      data <- filepath_split %>% read_function(...)
      lm_each_subsample(formula, family, data, nrow(data), B)
    })
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' split data into m parts of approximated equal sizes
split_sample <- function(data, m) {
  idx <- sample.int(m, NROW(data), replace = TRUE) # NROW over nrow so it doesn't break on vectors
  data %>% split(idx)
}

#' compute the estimates
lm_each_subsample <- function(formula, family, data, n, B) {
  replicate(B, glm_each_boot(formula, family, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
glm_each_boot <- function(formula, family, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, family, data, freqs)
}


#' estimate the regression estimates based on given the number of repetitions
glm1 <- function(formula, family, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- glm(formula, family = family, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

simplify_estimates <- function(fit) {
  fit$estimates %>% map(function(x) {
    df <- cbind(
      map_dfr(x, ~ data.frame(as.list(.$coef))),
      map_dfr(x, ~ data.frame(sigma = .$sigma))
    )
    names(df) <- c("(Intercept)", names(df)[-1])
    df
  })
}

# TODO: richer print information

#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", Reduce(paste, deparse(x$formula))) # R does not like capture.output
  cat("\n")
}

# TODO: maybe sigma, coefs should be stored in the blblm object?

#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  sigma <- mean(map_dbl(object$estimates, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - level
    limits <- object$estimates %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  map_mean(object$estimates, ~ map_cbind(., "coef") %>% rowMeans())
}


#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  out <- map_rbind(parm, function(p) {
    map_mean(object$estimates, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
