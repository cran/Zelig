terms.multiple<-function(x, data=NULL,...){
  object <- x
  termsexist<-attr(object,"terms")
  if(!(is.null(termsexist)))
    return (termsexist)
  
  nreq<-nrConstr<-nrEquationsNew<-0
  constr<-XconstrEqn<-variables<-termlabels<-depVars<-objectNew<-intercAttr<-depFactors<-list()
  depFactorVar<-depLevels<-namesConstr<-c()
  namesOfEquations<- names(object)
  nrEquations <-length(object)
  "%w/o%" <- function(x,y) x[!x %in% y]
  
  for (i in 1:nrEquations){
    TT<-terms.formula(object[[i]], specials=c("id","tag"))
    eqni<-object[[i]]                    
    namei<-namesOfEquations[[i]]            
    tagattr<-attr(TT,"specials")$tag         
    hastag<-!(is.null(tagattr))
    if (hastag){
      constrTmp<-c()
      for(j in 1:length(tagattr)){
        nrConstr<-nrConstr+1
        if(length(eqni)==3)
          ind<-tagattr[[j]]-1
        else
          ind<-tagattr[[j]]
        vind<-tagattr[[j]]+1
        parsedTag<-attr(TT,"variables")[[vind]]
        varname<-as.character(parsedTag[[2]])
        label<-as.character(parsedTag[[3]])
        namesConstr<-c(namesConstr,label)
        constr[[nrConstr]]<-c(i,label,varname)
        attr(TT,"term.labels")[[ind]]<-varname
        attr(TT,"variables")[[vind]]<-as.name(varname)         
        constrTmp<-c(constrTmp,varname)
      }
      XconstrEqn[[i]]<-constrTmp
    }
    if (length(eqni)==3){                               
      nrEquationsNew<-nrEquationsNew+1
      objectNew[[namei]]<-eqni
      nreq=nreq+1
      lhs<-eqni[[2]]
      if (length(lhs)>1 && lhs[[1]]=="id"){
        depVars[[namei]]<-lhs[[3]]
        depFactorVar<-c(depFactors,deparse(lhs[[2]]))
        depLevels<-c(depLevels,lhs[[3]])
      }else
      depVars[[namei]]<-deparse(eqni[[2]])
    }else{                            
      nrEquationsNew<-nrEquationsNew+1
      objectNew[[namei]]<-eqni
    }
    variables[[namei]]<-attr(TT,"variables")
    termlabels[[namei]]<-attr(TT,"term.labels")
    intercAttr[[namei]]<-attr(TT,"intercept")
  }
  namesOfEquations<-names(objectNew)
  myattr<-list()
  result<-objectNew
  if(length(constr)>0){
    namesConstr<-unique(namesConstr)
    constraints<-matrix(NA,nrow=nrEquationsNew,ncol=length(namesConstr),dimnames=list(namesOfEquations,namesConstr))
    for(i in 1:length(constr)){
      constri<-constr[[i]]
      eqind<-constri[[1]]
      eq<-namesOfEquations[as.numeric(eqind)]
      lab<-constri[[2]]
      constraints[eq,lab]<-constri[[3]]
    }
  }else
  constraints<-FALSE

  indVars<-unique(unlist(termlabels))
  if(length(depFactorVar) !=0)
    depFactors<-list("depFactorVar"=unique(unlist(depFactorVar)),"depLevels"=depLevels)
  else
    depFactors<-FALSE
  
  whiche<-which(lapply(termlabels,length)!=0)
  myattr$systEqns<-names(whiche)
  myattr$ancilEqns<-"%w/o%"(namesOfEquations,myattr$systEqns)
  
  myattr$variables<-variables
  myattr$term.labels<-termlabels
  myattr$indVars<-indVars
  
  myattr$depVars<-depVars
  myattr$depFactors<-depFactors
  myattr$constraints<-constraints
  myattr$response<-1
  myattr$intercept<-intercAttr
  attributes(result)<-myattr
  names(result)<-namesOfEquations
  class(result)<-c("terms","multiple","list")
  return(result)
}
