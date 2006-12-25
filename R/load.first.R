.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "Zelig"))
  ver <- packageDescription("Zelig", lib = mylib)$Version
  builddate <- packageDescription("Zelig", lib = mylib)$Date
  cat(paste("## \n##  Zelig (Version ", ver, ", built: ", builddate, ")\n", sep = "")) 
  cat("##  Please refer to http://gking.harvard.edu/zelig for full documentation \n",
      "##  or help.zelig() for help with commands and models supported by Zelig.\n##\n",
      sep="")
  if(!any(search()=="package:MASS"))
    require(MASS) 
  if(!any(search()=="package:boot"))
    require(boot) 
  options(digits = 4)
  library.dynam("stats")
}
