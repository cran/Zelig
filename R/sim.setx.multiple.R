sim.setx.multiple <- function(object, x, x1 = NULL, num = c(1000, 100),
                         bootstrap = FALSE, ...){
  if (any(class(object) == "MCMCZelig"))
    num <- nrow(object$coefficients)
  if (length(num) == 2) {
    if (!bootstrap)
      num <- num[1]
    else
      num <- num[2]
  }
  if(any(class(object[[1]])=="MI")) {
    dta <- eval((object[[1]])[[1]]$call$data, sys.parent())[[1]]
    by <- (object[[1]])[[1]]$call$by
  }
  else {	
    dta <- eval(object[[1]]$call$data, sys.parent())
    by <- object[[1]]$call$by
  }
  N <- length(object)
  res <- list()
  idx <- match(by, names(dta))
  lev <- names(object)
  for (i in 1:length(lev)) {
    dat <- dta[dta[[idx]] == lev[i],]
    numN <- round(num/N)
    res[[i]] <- sim(object[[i]], x = x[[i]], x1 = x1[[i]], num = numN,
                    bootstrap = bootstrap, ...)
    res[[i]]$nx <- nrow(dat)/nrow(dta)
  }
  class(res) <- "zelig.multiple"
  names(res) <- names(object)
  res
}







