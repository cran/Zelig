print.setx<-function(x, digits=getOption("digits"),
                     ...){
  attributes(x)$class<-NULL
  attributes(x)$assign<-NULL
  attributes(x)$contrasts<-NULL
  print.matrix(x, digits=digits, ...)
}
