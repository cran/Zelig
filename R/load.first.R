.onAttach <- function(...) {
  cat("\nPlease refer to http://gking.harvard.edu/zelig for full documentation \n",
      "or help.zelig() for help with commands and models supported by Zelig.\n\n",
      sep='')
  if(!any(search()=="package:MASS"))
    require(MASS) 
  if(!any(search()=="package:boot"))
    require(boot) 
  options(digits = 4)
}
