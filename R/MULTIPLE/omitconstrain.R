omitconstrain <-function(object, omit=NULL,constrain=NULL){
  
  if(class(object)[[1]]=="formula"){
    att <-attributes(terms(object))
    obj<-"formula"
  }
  else
    {
      obj<-"terms"
      att<-attributes(object)
    }
  expVar<-att$term.labels
  nrExpVariables<- length(expVar)
  nrEquations<-length(all.vars((att$variables)[[2]],unique=FALSE))
  attrList<-list()

  omitAttr <-matrix(0,nrEquations,nrExpVariables) 
  if (length(omit)>0){
    omitNames<-as.numeric(names(omit))
    if (max(omitNames)<= nrEquations ){ 
      for(i in 1:length(omitNames)){
        m<-omitNames[i]
        for(j in 1:length(omit[[as.character(m)]]))
          {
            n<-match(omit[[as.character(m)]][j],expVar)
            omitAttr[m,n]<-1
          }
      } 
    }
    else
      stop("\n\n The omit list you supplied as argument is not correct ! \n\n")
  }
  attrList[["omit"]]<-omitAttr

  if(length(constrain)>0){
    vtmp<-length(constrain[[1]])
    for (i in 1:length(constrain)){
      if (length(constrain[[i]]) != vtmp)
        stop("\n\nThe constrain list you supplies as argumet is not correct! \n\n")
      vtmp<-length(constrain[[i]])
    }
    J<-vtmp                              
    constrainAttr<-matrix(0,nrEquations,J)
    constrainNames <-as.numeric(names(constrain))
    if(max(constrainNames)<=nrEquations){
      for(i in 1:length(constrain)){
        for(j in 1:J){
          if(!(is.na(constrain[[i]][j]))){
            m<-constrainNames[i]
            n<-match(constrain[[i]][j],expVar)
            constrainAttr[m,j]<-n
          }
        }
      }
    }
    else
      stop("\n\n The constrain list you supplied as argument is not correct ! \n\n")
    attrList[["constrain"]]<-constrainAttr
  }
  attrList
}
