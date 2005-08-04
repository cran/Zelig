zelig2relogit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- mf$robust <- NULL
  if (is.null(mf$case.control))
    mf$case.control <- "prior"
  mf$formula <- as.formula(paste("cbind(", formula[[2]], ", 1 -", formula[[2]], ")",
                                 "~", deparse(formula[[3]]), sep = ""))
  if (is.null(mf$bias.correct))
    mf$bias.correct <- TRUE
  mf[[1]] <- as.name("relogit")
  as.call(mf)
}
