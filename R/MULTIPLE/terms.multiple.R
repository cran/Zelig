terms.multiple <- function (object,omit=NULL, constrain=NULL){

  if (any(class(object)=="multiple")){
    terms<-object$terms
    class(terms)<-c(class(terms),"multiple")
    return (terms)
  }

  terms<-terms.formula(object)

  attrList<-attributes(terms)
  tmp<-omitconstrain(object,omit,constrain)
  attrList[["omit"]]<-tmp[["omit"]]
  attrList[["constrain"]]<-tmp[["constrain"]]
  attributes(terms)<-attrList
  class(terms)<-c(class(terms),"multiple")
  terms
}

