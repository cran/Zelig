zelig2blogit <- function(formula, model, data, M, constrain = NULL,
                         omit = NULL, constant = 3, ...) {
  check <- library()
  if(any(check$results[,"Package"] == "VGAM")) 
    require(VGAM)
  else
    stop("Please install VGAM using \n     install.packages(\"VGAM\", CRAN = \"http://www.stat.auckland.ac.nz/~yee\")")
  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- as.name("vglm")
  mf$family <- as.name("blogit")
  mf$... <- NULL
  tmp <- cmvglm(formula, model, constrain, omit, constant, 3)
  mf$formula <- tmp$formula 
  mf$constraints <- tmp$constraints
  blogit <<- function() binom2.or(zero=NULL)
  mf$model <- mf$constrain <- mf$omit <- mf$constant <- mf$M <- NULL
  as.call(mf)
}
