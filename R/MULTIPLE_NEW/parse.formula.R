parse.formula<-function( listofformulas, req=NULL, opt=NULL, fixed=NULL){

nrrho<-0                                              # number of opt ???? replace nrrho with nopts
nreq<-0                                               # final number of equations (for req) i.e. rep contain more then one eq)
eqns<-list()                                          # list of req equations
rho<-list()                                           # list of opt equations ??? replace rho with opt???
fix<-list()
res<-list()                                           # the final result : list of equation + attributes

#if(!(is.null(fixed)) && fixed=="rho")
#  stop("Please chose a different value for fixed parameter")

if(class(listofformulas)=="formula")
  listofformulas<-list(listofformulas)


nreqns <-length(listofformulas)                       # total number of equations
for (i in 1:nreqns){
  eqni<-listofformulas[[i]]
   if (length(eqni)==3){                               # on left hand side we have something
      lhs<-deparse(eqni[[2]])
     rhs<-deparse(eqni[[3]])
     if(substr(lhs,1,5)!="cbind"){
       nreq=nreq+1
       e<-paste(lhs,"~",sep="")
       eqns[[nreq]]<-as.formula(paste(e,rhs,sep=""))
     }
     else
     { 
         lhs<-eqni[[2]]
         g<- all.vars(lhs)             # how many eq we have inside cbind
         for (j in 1:length(g)){
           nreq=nreq+1
           e<-paste(g[[j]],"~",sep="")
           eqns[[nreq]]<-as.formula(paste(e,rhs,sep=""))
         }
       }
   }
  else                            # if we dont have anyting means is rho
    {
      rhs<-deparse(eqni[[2]])
      nrrho=nrrho+1
      rho[[nrrho]]<-as.formula(paste("~",rhs,sep="")) 
    }
}

if (is.null(opt)){
  if (nrrho !=0)
    stop("number of equations does not match model inputs!")
}
else
  {
    if (nrrho < length(opt))
      if (length(opt)==1){
        nrrho=nrrho+1
        rho[[nrrho]]<-as.formula("~1")
      }
      else
        stop("number of equations does not match model inputs!")
    names(rho)<-opt
    if (nrrho > length(opt))
      if(length(opt)==1)
        names(rho)<-paste(opt,1:nrrho,sep="")
      else
        stop("number of equations does not match model inputs!")
  }

if(!(is.null(fixed)))
for(i in 1:length(fixed)){
  fix[[fixed[[i]]]]<-as.formula("~1")
}

if (is.null(req)){
  if (nreq !=0)
    stop("number of equations does not match model inputs!")
}
else
  {
    if (nreq < length(req))
        stop("number of equations does not match model inputs!")
    names(eqns)<-req
    if (nreq > length(req))
      if(length(req)==1)
        names(eqns)<-paste(req,1:nreq,sep="")
      else
        stop("number of equations does not match model inputs!")
  }
res<-c(eqns,rho,fix)
return(res)
}
