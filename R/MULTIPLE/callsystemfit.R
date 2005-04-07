callsystemfit<-function(formula,data,eqns,method,omit=NULL,constrain=NULL,...){

t<-terms.multiple(formula,omit,constrain)
out<-systemfit(data=data,eqns=eqns,method=method,...)
out$terms<-t
class(out)<-c(class(out),"multiple")
return (out)
}
