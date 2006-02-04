current.packages <- function(package){

  required.packages <- function(pack) { 
    mylib <- dirname(system.file(package = pack))
    description <- packageDescription(pack, lib = mylib)       
    depends <- description$Depends
    if (!is.null(depends)) {
      depends <- strsplit(depends, ", ")[[1]]
      Rdepends <- pmatch("R (", depends)
      if (is.na(Rdepends))
        Rdepends <- pmatch("R(", depends)
      if (!is.na(Rdepends)) 
        depends <- depends[-pmatch("R (", depends)]
    }
    suggests <- description$Suggests
    if (!is.null(suggests)) 
      suggests <- strsplit(suggests, ", ")[[1]]
    total <- c(depends, suggests)
    if (!is.null(total)) {
      conditions <- grep(")", total)
      if (length(conditions) > 0) { 
        for (i in conditions) 
          total[i] <- strsplit(total[i], " \\(")[[1]][1]
      }
      return(total)
    }
    else
      return(NULL)
  }
  old <- packages <- required.packages(package)

  for (zpack in packages) {
    new <- required.packages(zpack)
    tmp <- new[!(new %in% old)]
    old <- packages <- c(packages, tmp)
  }

  ver <- array()
  for (zpack in na.omit(packages)) { 
    mylib <- dirname(system.file(package = zpack))
    ver[zpack] <- packageDescription(zpack, lib = mylib)$Ver
  }
  ver[1] <- paste(paste(paste(R.Version()$major, R.Version()$minor, sep = "."),
                        R.Version()$status, sep = " "),
                  R.Version()$svn, sep = " svn: ")
  names(ver)[1] <- "R"
  vv <- as.matrix(ver)
  colnames(vv) <- "Version"
  noquote(vv)
}

