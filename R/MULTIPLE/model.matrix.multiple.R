model.matrix.multiple <- function (object,data,eqn=NULL,...){

 # print("model.matrix.multiple is called")
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

  terms1<-terms
  class(terms1)<-class(terms1)[class(terms1)!="multiple"]
  multiple<-model.matrix.default(terms1,data)
  
  
  attrList<-attributes(multiple)
  if(obj=="formula"){
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
        nrEquations<-dim(omitAttr)[[1]] 
      }
      
      if ("constrain" %in% names(attributes(object)))
        attrList[["constrain"]]<-attributes(object)$constrain
    }
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

  multiple

}

