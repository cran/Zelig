zelig2probit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- mf$probit <- NULL
  mf$formula <- as.formula(paste("cbind(", formula[[2]], ", 1 -", formula[[2]], ")",
                                 "~", deparse(formula[[3]]), sep = ""))
  mf[[1]] <- stats::glm
  mf$family <- binomial(link="probit")
  as.call(mf)
}
