model.frame.list <- function (object,data,...){
  if(class(object)[[1]]=="terms"){
    terms <-object
  }else{
    terms<-terms(object)
  }
  Xnames<-attr(terms,"indVars")
  rhs<-toBuildFormula(Xnames)
  if(!(is.null(rhs)))
    rhs<-(paste("~",rhs))
  else
    stop("NULL dataframe")
  Ynames<-attr(terms,"depVars")
  if(length(Ynames)>1){
    lhs<-toBuildFormula(Ynames,",")
    if (!(is.null(lhs))){
      lhs<-paste("cbind(",lhs)
      lhs<-paste(lhs,")")
    }
  }else{
    lhs=Ynames
  }
  lhs<-as.formula(paste(lhs,rhs))
  Y<-model.frame.default(lhs,data=data)
  result=Y
 # attr(terms,"response")<-1
  attr(result,"terms")<-terms
  result 
}

