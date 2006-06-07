zelig2ei.RxC <- function(formula, model, data, M, covar= NULL, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- as.name("callparamseiestim")
  tmp <- cmei(formula=formula, data=data, covar=covar)
  mf$data <- tmp$data
  #print(mf$data)
  mf$nR<-tmp$nR
  mf$nC <- tmp$nC

  if(!(is.null(covar))){
    mf$covar<-tmp$covar
  }
  mf$model<-mf$M<-NULL
  as.call(mf)
}
