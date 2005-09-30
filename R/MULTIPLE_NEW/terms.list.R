terms.list<-function(object,...){

nreq<- 0                                              
reqEqns<-list()                                       
optEqns<-list()                                       
constr<-list()
XconstrEqn<-list()                                    
variables<-list()                                     
termlabels<-list()                                    
depVars<-c()                                     
nrConstr<-0
namesConstr<-c()
namesOfEquations<- names(object)
nrEquations <-length(object)                          
nrEquationsNew<-0
objectNew<-list()
intercAttr<-list()

trim<-function(s){
  return (sub(" *([^ ]+) *", "\\1", s))
}


parseTag<-function(s){
  rml<-sub(".*\\(","",s)
  rmr<-sub("\\).*","",rml)
  r<-unlist(strsplit(rmr,",",fixed=TRUE))
  r<-trim(r)
  if(substr(r[[2]],1,1)!="\"" || substr(r[[2]],nchar(r[[2]]),nchar(r[[2]]))!="\"")
    stop( "the name of the parameters should be label (enclosed in quotation marks ...)")
  r[[2]]<- substr(r[[2]],2,nchar(r[[2]])-1)                                              
  return(r)
}



for (i in 1:nrEquations){
  TT<-terms.formula(object[[i]], specials="tag")
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
      parsedTag<-parseTag(attr(TT,"term.labels")[[ind]])
      namesConstr<-c(namesConstr,parsedTag[[2]])
      constr[[nrConstr]]<-c(i,parsedTag[[2]],parsedTag[[1]])
      attr(TT,"term.labels")[[ind]]<-parsedTag[[1]]                 
      ind<-tagattr[[j]]+1
      attr(TT,"variables")[[ind]]<-as.name(parsedTag[[1]])         
      constrTmp<-c(constrTmp,parsedTag[[1]])
    }
    XconstrEqn[[i]]<-constrTmp
  }

  if (length(eqni)==3){                               
    nrEquationsNew<-nrEquationsNew+1
    objectNew[[namei]]<-eqni
    nreq=nreq+1
    reqEqns[[namei]]<-eqni
    depVars<-c(depVars,deparse(eqni[[2]]))
  }else{                            
      nrEquationsNew<-nrEquationsNew+1
      objectNew[[namei]]<-eqni
      optEqns[[namei]]<-eqni
  }
  variables[[namei]]<-attr(TT,"variables")
  termlabels[[namei]]<-attr(TT,"term.labels")
      intercAttr[[namei]]<-attr(TT,"intercept")
}

namesOfEquations<-names(objectNew)

myattr<-list()
result<-objectNew
reqnames<-names(reqEqns)

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

myattr$reqEqns<-reqEqns
myattr$optEqns<-optEqns
myattr$variables<-variables
myattr$term.labels<-termlabels
myattr$indVars<-indVars

myattr$depVars<-unlist(depVars)
myattr$constraints<-constraints
myattr$response<-1
myattr$intercept<-intercAttr
attributes(result)<-myattr
names(result)<-namesOfEquations
class(result)<-c("terms","list")
return(result)
}
