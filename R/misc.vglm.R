"$.vglm"<-"$.summary.vglm"<-function(a, b){
  if(is.na(pmatch(b, slotNames(a)))){
    pmt<-pmatch(b, names.default(a))
    if(is.na(pmt))
      stop(paste("no such object exists in", a))
    else
      a[[pmt]]
  }
  else
    slot(a, b)
}

"$<-.vglm"<-"$<-summary.vglm"<-function(a, b, value){
  if(is.na(pmatch(b, slotNames(a)))){
    tmp<-class(a)
    class(a)<-NULL
    if(length(a)==0){
      a[[1]]<-value
      names(a)[1]<-b
    }
    else{
      pmt<-pmatch(b, names(a))
      if(is.na(pmt)){
        n<-length(names(a))
        a[[n+1]]<-value
        names(a)[n+1]<-b
      }
      else
        a[[pmt]]<-value
    }
    class(a)<-tmp
  }
  else
    slot(a, b)<-value
  a
}
