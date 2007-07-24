zelig2bprobit <- function(formula, model, data, M, constant = 3, ...) {
  check <- library()
  if(any(check$results[,"Package"] == "VGAM")) 
    require(VGAM)
  else
    stop("Please install VGAM using \n     install.packages(\"VGAM\", repos = \"http://www.stat.auckland.ac.nz/~yee\")")
  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- VGAM::vglm
  mf$family <- as.name("bprobit")
   formula<-parse.formula(formula,model)
  tmp <- cmvglm(formula, model, constant, 3)
  mf$formula <- tmp$formula
  mf$constraints <- tmp$constraints
  bprobit <<- function() binom2.rho(zero=NULL)
  mf$model <- mf$constant <- mf$M <- NULL

  as.call(mf)
}
