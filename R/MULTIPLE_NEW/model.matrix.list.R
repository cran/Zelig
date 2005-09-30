model.matrix.list <- function (object,data,shape="compact",eqn=NULL,...){

  if((shape != "compact") && (shape != "array") && (shape !="stacked"))
    stop("wrong shape argument!")
  
  if(class(object)[[1]]=="terms"){
    terms <-object
  }
  else
    {
      terms<-terms(object)
    }
  if (!(all(eqn %in% names(terms))))
    stop("wrong eqn argument")
  
  constraints<-attr(terms,"constraints")
  intercAttr<-attr(terms,"intercept")
  termlabels<-attr(terms,"term.labels")
  if (is.null(eqn))
    eqn=names(terms)
  
  if (length(eqn)==1)
    shape="compact"
  Xnames<-unique(unlist(termlabels[eqn]))
  rhs<-toBuildFormula(Xnames)
  if(!(is.null(rhs)))
    rhs<-as.formula(paste("~",rhs))
  
  if (shape=="compact"){
    result=model.matrix.default(rhs,data=data)
    if(all(intercAttr==0)){
      result= result[,colnames(result)!="(Intercept)"]
    }
    attr(terms,"parameters")<-colnames(result)
    attr(result,"terms")<-terms
    return(result)
  }
  intercAttr<-intercAttr[eqn]
  termlabels<-termlabels[eqn]
  nrEquations<-length(eqn)
  result<-list()
  for(i in 1:nrEquations)
    result[[eqn[[i]]]]<-c()

  ronames<-rownames(data)
  ronr<-nrow(data)
  
  parameters<-c()
  # start with intercept matrix
  matrixInterc<-list()
  howmanyinterc<-length(intercAttr[intercAttr!=0])

  if(howmanyinterc>0){
    for (i in 1:nrEquations){
      if(length(termlabels[[eqn[[i]]]])>0){
        matrixInterc[[eqn[[i]]]]<-matrix(0, nrow=ronr,ncol=howmanyinterc,dimnames=list(ronames,names(intercAttr[intercAttr!=0])))
        if(eqn[[i]] %in% names(intercAttr[intercAttr!=0]))
          matrixInterc[[eqn[[i]]]][,eqn[[i]]]<-1
        colnames(matrixInterc[[i]])<- paste("(Intercept)",colnames(matrixInterc[[i]]),sep=":")
      }
    }
    result<-matrixInterc
    parameters<-c(parameters,colnames(matrixInterc[[1]]))
  }
  matrixTmp<-list()
  newXnames<-list()

  for(i in 1:nrEquations){
    fi<-toBuildFormula(termlabels[[eqn[[i]]]])
    if(!(is.null(fi))){
      fi<-as.formula(paste("~",fi))
      tmp<-model.matrix.default(fi,data=data)
      matrixTmp[[eqn[[i]]]]<-tmp[,2:ncol(tmp)]
      newXnames[[eqn[[i]]]]<-colnames(matrixTmp[[eqn[[i]]]])
    }
  }
  
  parol<-c()
  if (is.matrix(constraints)){
    constraints<-matrix(constraints[eqn,],nrow=length(eqn),ncol=ncol(constraints),dimnames=list(eqn,colnames(constraints)))
    matrixConstr<-list()
    paramConstr<-c()
    
    for (i in 1:ncol(constraints)){
      if (any(!(is.na(constraints[,i])))){
        paramConstr<-c(paramConstr,colnames(constraints)[[i]])
      }
    }
    for (k in 1:length(paramConstr)){
      tmp<-paramConstr[[k]]
      for (m in 1:nrEquations){
        if(!(is.na(constraints[eqn[[m]],paramConstr[[k]]])))
          tmp<-paste(tmp,eqn[[m]],sep=":")
      }
      parol<-c(parol,tmp)
    }
  
    for (i in 1:nrEquations){
      if(length(termlabels[[eqn[[i]]]])>0){
        matrixConstr[[eqn[[i]]]]<-matrix(0,nrow=ronr,ncol=length(paramConstr),dimnames=list(ronames,paramConstr))
        
        for(j in 1:length(paramConstr)){
          tmpconstrv<-constraints[eqn[[i]],paramConstr[[j]]]
          if(!(is.na(tmpconstrv))){
            matrixConstr[[eqn[[i]]]][,paramConstr[[j]]]<-matrixTmp[[eqn[[i]]]][,tmpconstrv]
            tmp<- newXnames[[eqn[[i]]]]
            newXnames[[eqn[[i]]]]<-tmp[tmp!=tmpconstrv]
            tmp<-matrixTmp[[eqn[[i]]]]
            matrixTmp[[eqn[[i]]]]<-as.matrix(tmp[,newXnames[[eqn[[i]]]]])
          }
        }
      }
    }
  # if there is been not intercept then this would be the first .. otherwise cbind it
    if(length(result)==0)
      result<-matrixConstr
    else
      for(i in 1:nrEquations)
        result[[eqn[[i]]]]<-cbind(result[[eqn[[i]]]],matrixConstr[[eqn[[i]]]])
    parameters<-c(parameters,parol)
  }

  for(i in 1:nrEquations){
    tmp<- newXnames[[eqn[[i]]]]
    if(length(tmp)>0){
      newXnames[[eqn[[i]]]]<-paste(tmp,eqn[[i]],sep=":")
      colnames(matrixTmp[[eqn[[i]]]])<-newXnames[[eqn[[i]]]]
    }
  }
  matrixTheRest<-list()
  namesTheRest<-unique(unlist(newXnames ))
  
  for (i in 1:nrEquations){
    if(length(newXnames[[eqn[[i]]]])>0){
      matrixTheRest[[eqn[[i]]]]<-matrix(0,nrow=ronr,ncol=length(namesTheRest),dimnames=list(ronames,namesTheRest))
      matrixTheRest[[eqn[[i]]]][,newXnames[[eqn[[i]]]]]<-matrixTmp[[eqn[[i]]]]
    }
  }
      # if there is been not intercept then this would be the first .. otherwise cbind it
  if(length(result)==0)
    result<-matrixTheRest
  else
    for(i in 1:nrEquations)
      result[[eqn[[i]]]]<-cbind(result[[eqn[[i]]]],matrixTheRest[[eqn[[i]]]])
  parameters<-c(parameters,colnames(matrixTheRest[[1]]))
  
  if(shape=="array"){
    res<-array(0,dim=c(ronr,length(parameters),length(result)),dinames<-list(ronames,colnames(result[[1]]),names(result)))
    for (i in 1:length(result))
      res[,,names(result)[[i]]]<-result[[i]] 
  }

  if(shape=="stacked"){
    res<-result[[1]]
    if(length(result)>1)
      for(i in 2:length(result))
        res<-rbind(res,result[[i]])
  }
  
  for(i in 1:nrEquations)
    if(length(termlabels[[i]])==0)
      parameters<-c(parameters,eqn[[i]])
  
  attr(terms,"parameters")<-parameters
  
  
  
  attr(res,"terms")<-terms
  return(res)
}

