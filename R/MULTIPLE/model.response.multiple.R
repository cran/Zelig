model.response.multiple<-function(data,eqn=NULL){

response<-model.response(data)
dimnames<-dimnames(response)
if (!is.null(eqn)){
  response<-as.data.frame(response[,eqn])
  dimnames(response)<-list(dimnames[[1]],dimnames[[2]][eqn])
                         }
response

}
