  toBuildFormula<-function(Xnames,sepp="+"){
    lng<-length(Xnames)
    rhs<-NULL
    if (lng!=0){
      if(lng==1){
        rhs=Xnames
      }else{
        for (j in 1:(lng-1)){
          rhs<-paste(rhs,as.name(Xnames[[j]]))
          rhs<-paste(rhs,sepp)
        }
        rhs<-paste(rhs,Xnames[[lng]])
      }
    }
    return (rhs)
  }

#mode=1 model.matrix
#mode=2 model.frame
multilevel<-function(tt,data,mode,...){
  if(!(mode %in% c(1,2)))
    stop("Wrong mode argument")
res<-list()
  eqn<-attr(tt,"systEqns")
  subs<-attr(tt,"subs")
depVars<-attr(tt,"depVars")

  nrEquations<-length(eqn)
  termlabels<-attr(tt,"term.labels")
for(i in 1:nrEquations){
  rhs<-toBuildFormula(termlabels[[eqn[[i]]]],"+")
  if(!is.null(rhs))
    rhs<-paste("~",rhs)
  else
    rhs<-"~1"
  Ynamei<-depVars[[eqn[[i]]]]
  if(!(Ynamei %in% colnames(subs)))
    lhs<-Ynamei
  else
    lhs<-NULL
  f<-as.formula(paste(lhs,rhs))
  if(mode==1)
    res[[eqn[[i]]]]<-model.matrix.default(f,data[[eqn[[i]]]])
  #    res[[eqn[[i]]]]<-f
  else
    res[[eqn[[i]]]]<-model.frame.default(f,data[[eqn[[i]]]])
 # res[[eqn[[i]]]]<-f
}
attr(res,"terms")<-tt
return(res)
}
