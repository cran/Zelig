cmvglm <- function(formula, model, constrain, omit, constant, ndim){ 
  tt<-terms(formula)
  vars<-c("(Intercept)", attr(tt, "term.labels"))
  cm<-vector("list", length(vars))
  names(cm)<-vars
    for(i in 1:length(cm))
      cm[[i]]<-diag(1, ndim)
  if(!is.null(constrain)){
    tmp <- sort(names(constrain))
    for (i in 1:length(constrain[[tmp[1]]])) {
      ci <- NULL
      counter <- c("1", "2", "3")
      for (j in 1:3) 
        if (is.null(constrain[[counter[j]]]))
          ci <- c(ci, NA)
        else
          ci <- c(ci, constrain[[counter[j]]][i])
      if (is.null(na.omit(ci)) || length(unique(na.omit(ci)))!=1)
        stop("invalid input for constrain")
      minj <- match(FALSE, is.na(ci))
      whatvar <- pmatch(unique(na.omit(ci)), names(cm))
      for (j in 1:3)
        if (!is.na(ci[j])) {
          cm[[whatvar]][j,j]<-0
          cm[[whatvar]][j,minj]<-1
        }
    }
  }
  if(!is.null(omit)){
    tmp <- sort(names(omit))
    for(i in 1:length(omit)){
      omiti <- omit[[tmp[i]]]
      for (j in 1:length(omiti))
        cm[[pmatch(omiti[j], names(cm))]][i,i]<-0
    }
  }
  if(!is.null(constant))
    for(i in 1:length(constant))
      for(j in 1:length(cm))
        if(names(cm)[j]!="(Intercept)")
          cm[[j]][constant[i],]<-matrix(0, ncol=ncol(cm[[j]]))
  for(i in 1:length(cm))
    cm[[i]]<-as.matrix(cm[[i]][,apply(cm[[i]], 2, sum)!=0])
  list("formula"=formula, "constraints"=cm)
}
