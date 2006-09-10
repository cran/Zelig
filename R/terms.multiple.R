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
    
    attrTTvars<-attr(TT,"variables")
    newVars<-list()             # has tag so make a new list of variables
    newTermslbls<-c()
    eqni<-object[[i]]                    
    namei<-namesOfEquations[[i]]            
    tagattr<-attr(TT,"specials")$tag         
    hastag<-!(is.null(tagattr))
    if (hastag){
      indxV<-indxV<-1
      constrTmp<-c()
      for(j in 1:length(tagattr)){
        taglabels<-c()
        if(length(eqni)==3)
          lind<-tagattr[[j]]-1
        else
          lind<-tagattr[[j]]
        vind<-tagattr[[j]]+1
        for(v in indxV:(vind))
          newVars<-c(newVars,attrTTvars[[v]])    # add all vars prior to tag into new list of vars
        newVars[[length(newVars)]]<-NULL         # delete the last element (which is tag(...)
        indxV<-vind+1
        parsedTag<-attrTTvars[[vind]]
        if(length(parsedTag)==3){   # there is comma there
          varname<-as.name(parsedTag[[2]])
          newVars<-c(newVars,varname)
          after.comma.vars<-all.vars(parsedTag[[3]])
          print(parsedTag)
          print(after.comma.vars)
          if(length(after.comma.vars)==2){       # tag(z,gamma|state)
            label<-as.character(after.comma.vars[[1]])
            varname<-as.name(after.comma.vars[[2]])   # one more variable here (state)
            newVars<-c(newVars,varname)
          }else{                                 # tag(z,gamma)  ; we are talkig about constraints here
            varname<-as.character(parsedTag[[2]])
            nrConstr<-nrConstr+1
            label<-as.character(after.comma.vars)
            namesConstr<-c(namesConstr,label)
            constr[[nrConstr]]<-c(i,label,varname)
            constrTmp<-c(constrTmp,varname)
          }
        }else {                                         # tag(z|state)
          after.comma.vars<-all.vars(parsedTag[[2]])
          if(length(after.comma.vars)>1){
            for(v in after.comma.vars)
              newVars<-c(newVars,as.name(v))
          }else
          stop("wrong use of \"tag()\" function. Please see Zelig documentation for more information")
        }
      }
      if(length(attrTTvars)>vind){      # is there any var remaining after tags? add them
        for(v in (vind+1):length(attrTTvars))
          newVars<-c(newVars,attrTTvars[[v]])
      }
      XconstrEqn[[i]]<-constrTmp
      newVars<-unique(newVars)               # finish with tags, make newVars unique
      
    } else
    newVars<-attrTTvars
    
    if (length(eqni)==3){
      attr(TT,"term.labels")<-as.character(newVars)[-(1:2)]
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
      attr(TT,"term.labels")<-as.character(newVars)[-1]
      nrEquationsNew<-nrEquationsNew+1
      objectNew[[namei]]<-eqni
    }
    attr(TT,"variables")<-as.call(newVars)
    variables[[namei]]<-attr(TT,"variables")
    termlabels[[namei]]<-attr(TT,"term.labels")
    intercAttr[[namei]]<-attr(TT,"intercept")
  }
  
  namesOfEquations<-names(objectNew)
  myattr<-list()
  result<-objectNew
  subs<-constraints<-FALSE
  
  if(length(constr)>0){
    dvars<-unique(unlist(depVars))
    namesConstr<-unique(namesConstr)
    namesC<-namesConstr %w/o% dvars
    namesS <-namesConstr %w/o% namesC
    nrC<-nrS<-0
    constrC<-constrS<-list()
    rownamesS<-c()
    for(i in 1:length(constr)){
      constri<-constr[[i]]
      if(constri[[2]] %in% dvars){
        nrS=nrS+1
        constrS[[nrS]]<-constr[[i]]
        rownamesS<-c(rownamesS,namesOfEquations[[which(constri[[2]] ==dvars)]])        
      }else{
        nrC=nrC+1
        constrC[[nrC]]<-constr[[i]]
      }
    }
    if(length(constrC)>0){
      constraints<-matrix(NA,nrow=nrEquationsNew,ncol=length(namesC),dimnames=list(namesOfEquations,namesC))
      for(i in 1:length(constrC)){
        constri<-constrC[[i]]
        eqind<-constri[[1]]
        eq<-namesOfEquations[as.numeric(eqind)]
        lab<-constri[[2]]
        constraints[eq,lab]<-constri[[3]]
      }
    }
    if(length(constrS)>0){  #subs
      subs<-matrix(NA,nrow=length(rownamesS),ncol=length(namesS),dimnames=list(rownamesS,namesS))
      for(i in 1:length(constrS)){
        constri<-constrS[[i]]
        lab<-constri[[2]]
        subs[rownamesS[[i]],lab]<-constri[[3]]
      }
    }
  }                  
  
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
  myattr$subs<-subs
  myattr$response<-1
  myattr$intercept<-intercAttr
  attributes(result)<-myattr
  names(result)<-namesOfEquations
  class(result)<-c("terms","multiple","list")
  return(result)
}
