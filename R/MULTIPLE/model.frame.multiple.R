model.frame.multiple <- function (object,data,eqn=NULL,...){
  if(class(object)[[1]]=="formula"){
    terms <-terms(object)
    obj<-"formula"
  }
  else
  {
    obj<-"terms"
    terms<-object
  }
  att<-attributes(terms)
  expVar<-att$term.labels
  nrExpVariables<- length(expVar)
  nrEquations<-length(all.vars((att$variables)[[2]],unique=FALSE))

  
  multiple<-model.frame.default(terms,data)
  termsAttr<-attr(multiple,"terms")
  attrList<-attributes(termsAttr)
  if(obj=="formula"){
   # nrEquations<- length(all.vars(terms))-nrExpVariables
    if (hasArg(omit))
      omitLst=omit
    else
      omitLst=NULL
    if(hasArg(constrain))
      constrainLst=constrain
    else
      constrainLst=list()
      omitconsLst<-omitconstrain(object,omitLst,constrainLst)
      attrList<-c(attrList,omitconsLst)
  } # end of "if obj="formula""
  else
    {
      if ("omit" %in% names(attributes(object))){
        attrList[["omit"]]<-attributes(object)$omit
        omitAttr<-attributes(object)$omit
       # nrEquations<-dim(omitAttr)[[1]]
      }
      if ("constrain" %in% names(attributes(object)))
        attrList[["constrain"]]<-attributes(object)$constrain
    }
# construct eqn attribute
  if (!is.null(eqn))
    {
      eqnattr<-multiple[,c(1:dim(multiple)[[2]])* c(1,as.numeric(!(attrList[["omit"]][eqn,])))]
      attrname<-paste("eqn",eqn,sep="")
      multiple<-eqnattr
    }
  else
    {
      for (i in 1:nrEquations){
        eqntmp<-paste("eqn",i,sep="")
        attrtmp<-c(1:dim(multiple)[[2]])* c(1,as.numeric(!(attrList[["omit"]][i,])))
       attrtmp1<-attrtmp[attrtmp !=0]
        attr(multiple,eqntmp)<-attrtmp1
      }
    }

  attributes(attr(multiple,"terms"))<-attrList
  multiple


}

