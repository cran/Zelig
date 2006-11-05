###
## Creates a Zelig class of type S4 which inherit
## from the class of "object".
## If class of the object is "XXX" then the new class
## will be called "ZeligS4XXX"
## A new object is created from the new class and
## all the slots of the object
## input   : an object of class S4
## output  : a new object of class s4 with 2 additional
##           slots


create.ZeligS4 <- function(object){

  if(isS4(object)) {
    s4ClassName<- class(object)[[1]]
    newClassName <- paste("ZeligS4",s4ClassName,sep="")
    tmp <- paste("setClass(\"",newClassName,"\",representation(\"",s4ClassName,"\",zelig.data=\"data.frame\",zelig=\"character\"),where=.GlobalEnv)",sep="")
    
    ##create the new class
    eval(parse(text=tmp))
    
    ## initialize a new object of that class
    tmp<- paste("new(\"",newClassName,"\")",sep="")

    res<-eval(parse(text=tmp))
    
    ## copy over all the slots from teh existing object
    ## to the new object
    for(s in slotNames(object)){
      slot(res,s)<-slot(object,s)
    }
    return (res)
  } else {
    return(object)
    ##  error("\"object\" should be of class S4")
  }
}


"$.ZeligS4vglm" <- "$.summary.ZeligS4vglm" <- function(a, b){
  if(is.na(pmatch(b, slotNames(a))))
    stop(paste("no such slot exists in", a))
  else
    slot(a, b)
}

"$<-.ZeligS4vglm" <- "$<-.summary.ZeligS4vglm" <- function(a, b, value){
  if(is.na(pmatch(b, slotNames(a))))
    stop(paste("no such slot exists in", a))
  else
    slot(a, b)<-value
  return(a)
}
