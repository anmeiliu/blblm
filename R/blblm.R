#' @import purrr
#' @import stats
#' @import utils
#' @import future
#' @importFrom magrittr %>%
#' @details
#' Generalized Linear Models with Bag of Little Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' Bag of Little Bootstraps General Linear Model
#'
#' This function implements the bag of little bootstraps for the general linear
#' model.
#'
#' Linear and logistic regression are properly supported, but if your
#' data is appropriately formed, there's no reason you can't use other general
#' linear models with blbglm(). (Corrections to predictions, where necessary,
#' are only made for the logistic case.)
#'
#' Parallelized processing is supported through future::plan(). blbglm()
#' defaults to using future_map(), respecting any plan() the user has set.
#' However, disabling the use of future_map() for sequential processing is
#' usually faster; set use_plan = FALSE for this.
#'
#' blbglm() can also work with filepaths in lieu of a data object. Provide a
#' vector of filepaths and blbglm() will read them for you. (Filepath reading
#' is faster when blbglm() is running in a parallelized mode.) In this case,
#' each file is considered to be one subsample (and m is ignored).
#'
#' By default, blbglm() assigns an equal number of observations to each
#' subsample (ties are broken in favor of earlier numbered subsamples). It is
#' possible to disable this behavior so that blbglm() assigns each observation
#' to a subsample at random, without regard for the number of observation in
#' each subsample. The minimum number of observations in each sample is
#' controlled by min_subsample_size, but it should be at least 2 + the number
#' of dependent variables.
#'
#' @param formula The formula to use for the linear models.
#' @param family The family of the linear models (see glm()). Defaults to
#'   gaussian() for linear regression.
#' @param data An object containing data (which can be subsetted and passed to
#'   glm()).
#' @param filepaths A vector or list of filepaths to read from.
#' @param read_function The function to use to read files.
#' @param m The number of subsamples to create.
#' @param B The number of bootstraps for each subsample.
#' @param min_subsample_size The minimum size of each subsample. For small
#'   numbers of observations per subsample, mitigates weird behaviors from
#'   fitting glm() on small data sets.
#' @param even_split Whether to split subsamples so that they are as equally
#'   sized as possible.
#' @param use_plan Whether to use the plan set by future::plan(). use_plan =
#'   FALSE is faster for sequential processing.
#' @param ... Additional arguments to pass to read_function.
#'
#' @return A blbglm object, containing the estimates from each bootstrap, the
#'   formula, and the family.
#'
#' @export
#'
#' @examples
#' blbglm(mpg ~ wt, data = mtcars, B = 100, even_split = TRUE, use_plan = FALSE)
#' blbglm(Species ~ Sepal.Length, family = binomial(), data = iris,
#' m = 3, B = 100, min_subsample_size = 30)
blbglm <- function(formula, family = gaussian(), data = NULL, filepaths = NULL, read_function = read.csv, m = 10, B = 5000, min_subsample_size = NULL, even_split = NULL, use_plan = TRUE, ...) {
  if (is.null(data) & is.null(filepaths)) {
    stop("Neither data nor filepaths to data provided")
  }
  if (!is.null(data) & !is.null(filepaths)) {
    warning("Both data and filepaths specified, using data")
  }
  if (!is.null(filepaths) & length(filepaths) != m) {
    warning("Number of filepaths provided is not the same as number of splits, using file-based splits")
  }
  if (!is.null(filepaths) & !is.null(min_subsample_size)) {
    warning("Cannot specify min_subsample_size when using file-based splits")
  }
  if (!is.null(filepaths) & !is.null(even_split)) {
    warning("Cannot specify even_split when using file-based splits")
  }
  if (use_plan & grepl("sequential", deparse(attributes(plan())$call))) {
    warning("Using a sequential plan; this is usually slower than not using a plan (set use_plan = FALSE to use no plan)")
  }

  if (is.null(filepaths)) {
    if (is.null(min_subsample_size)) {
      if (is.null(even_split)) {
        even_split = TRUE
      } else if (!even_split) {
        min_subsample_size = length(all.vars(formula)) + 1
        message(paste("Using minimum subsample size ="), min_subsample_size)
      }
    } else {
      if (min_subsample_size * m > nrow(data)) {
        stop("min_subsample_size times m must be less than or equal to number of observations")
      } else if (!is.null(even_split)) {
        if (even_split) {
          warning("Cannot specify min_subsample_size when using even splits; ignoring min_subsample_size")
        }
      } else {
        even_split = FALSE
      }
    }
  }

  if (use_plan) {
    active_map <- furrr::future_map
  } else {
    active_map <- map
  }

  if (!is.null(data)) {
    data_list <- split_sample(data, m, min_subsample_size, even_split)
    estimates <- active_map(data_list, ~ glm_each_subsample(formula, family, ., nrow(.), B))
  } else {
    estimates <- active_map(filepaths, function(filepath_split) {
      data <- filepath_split %>% read_function(...)
      glm_each_subsample(formula, family, data, nrow(data), B)
    })
  }
  res <- list(estimates = estimates, formula = formula, family = family)
  class(res) <- "blbglm"
  invisible(res)
}

#' Randomly split data into m parts
#'
#' @param data A data object (dataframe, vector, etc.) to split.
#' @param m The number of splits to create.
#' @param min_subsample_size The minimum size of each split.
#' @param even_split Whether to create splits so they are as similarly-sized as possible.
#'
#' @return A list containing m subsets of data.
split_sample <- function(data, m, min_subsample_size, even_split) {
  if (even_split) {
    idx <- sample(rep_len(1:m, NROW(data)))
  } else {
    idx <- sample.int(m, NROW(data), replace = TRUE) # NROW over nrow so it doesn't break on vectors
    while ((sum(table(idx) < min_subsample_size)) > 0) {
      idx <- sample.int(m, NROW(data), replace = TRUE)
    }}
  data %>% split(idx)
}

#' Perform bootstraps with glm()
#'
#' @param formula The formula to use for the linear models.
#' @param family The family of the linear models (see glm()).
#' @param data An object containing data (which can be passed to glm()).
#' @param n The number of observations in the data.
#' @param B The number of bootstraps.
glm_each_subsample <- function(formula, family, data, n, B) {
  replicate(B, glm_each_boot(formula, family, data, n), simplify = FALSE)
}


#' Efficiently compute the estimate of glm() parameters for one bootstrap
#'
#' @param formula The formula to use for the linear models.
#' @param family The family of the linear models (see glm()).
#' @param data An object containing data (which can be passed to glm()).
#' @param n The number of observations in the data.
glm_each_boot <- function(formula, family, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, family, data, freqs)
}


#' Efficiently estimate glm() parameters based on observation frequency
#'
#' @param formula The formula to use for the linear models.
#' @param family The family of the linear models (see glm()).
#' @param data An object containing data (which can be passed to glm()).
#' @param freqs The frequency of each observation.
glm1 <- function(formula, family, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- glm(formula, family = family, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit, freqs))
}


#' Extract the coefficients from a model fit
#'
#' @param fit The model fit to be extracted from.
blbcoef <- function(fit) {
  coef(fit)
}


#' Compute sigma based on a model fit and observation weights
#'
#' The sigma computed here is based on the efficient bootstrap computation used
#' above, i.e. it takes into account the weights as separate observations.
#' fit$weights isn't used because it provides misleading results for
#' non-Gaussian GLMs, but fit$residuals is used to provide residuals that
#' generalize between GLM forms.
#'
#' @param fit The model fit to be extracted from.
#' @param weights The weight of each observation in the model.
blbsigma <- function(fit, weights) {
  p <- fit$rank
  e <- fit$residuals
  w <- weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' @export
#' @method print blbglm
print.blbglm <- function(x, ...) {
  cat("blbglm model: ")
  print(x$formula)
  cat("\ncoefficients:\n")
  print(coef(x))
  cat("\nsigma: ")
  cat(sigma(x), "\n")
}

#' @export
#' @method sigma blbglm
sigma.blbglm <- function(object, confidence = FALSE, level = 0.95, ...) {
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
#' @method coef blbglm
coef.blbglm <- function(object, ...) {
  map_mean(object$estimates, ~ map_cbind(., "coef") %>% rowMeans())
}


#' @export
#' @method confint blbglm
confint.blbglm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  out <- map_rbind(parm, function(p) {
    map_mean(object$estimates, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2), na.rm = TRUE))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @export
#' @method predict blbglm
predict.blbglm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  X <- model.matrix(reformulate(attr(terms.formula(object$formula, data = new_data), "term.labels")), new_data)
  logit <- ifelse(class(object$fit) == "function", formals(object$fit)$link == "logit",
                  ifelse(class(object$fit) == "family", object$fit$link == "logit", FALSE))
  if (logit) {
    pred_fun <- function(x) {
      logit_pred <- exp(X %*% x$coef)/(1 + exp(X %*% x$coef))
      if(is.infinite(logit_pred)) {
        sign(logit_pred)
      } else {
        logit_pred
      }
    }
  } else {
    pred_fun <- function(x) {
      X %*% x$coef
    }
  }
  if (confidence) {
    map_mean(object$estimates, ~ map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  } else {
    map_mean(object$estimates, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
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
