toBuildFormula<-function(Xnames,sepp="+"){
  lng<-length(Xnames)
  rhs=NULL
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
