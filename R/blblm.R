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


#' @export
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
        message("Using default minimum subsample size = 3")
        min_subsample_size = 3
      }
    } else {
      if (!is.null(even_split)) {
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

#' split data into m parts of approximated equal sizes
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

#' compute the estimates
glm_each_subsample <- function(formula, family, data, n, B) {
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
  list(coef = blbcoef(fit), sigma = blbsigma(fit, freqs))
}


#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
blbsigma <- function(fit, freqs) {
  p <- fit$rank
  e <- fit$residuals
  w <- freqs
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
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
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
