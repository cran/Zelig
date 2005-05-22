help.zelig <- function (...)  {
  zipped <- FALSE
  loc <- NULL
  name <- c(as.character(substitute(list(...))[-1]), list)[[1]]
  if (length(name) == 0) 
    loc <- "http://gking.harvard.edu/zelig"
  paths <- .find.package("Zelig")
  if (length(paths) > 1)
    warning(paste("Zelig installed in", length(paths), "locations.  Using\n     ", paths[1]))
  path <- paths[1]
  path <- file.path(path, "data")
  if (tools::file_test("-f", file.path(path, "Rdata.zip"))) {
    zipped <- TRUE
    if (tools::file_test("-f", fp <- file.path(path, "filelist"))) 
      files <- file.path(path, scan(fp, what = "", quiet = TRUE))
    else 
      stop(gettextf("file 'filelist' is missing for directory '%s'", 
                    path), domain = NA)
  }
  else 
    files <- list.files(path, full = TRUE)
  files <- files[which(regexpr("url", files) > 0)]
  if (length(files) == 0)
    loc <- "http://gking.harvard.edu/zelig"
  else {
    zfile <- array()
    for (f in 1:length(files)) {
      if (zipped) 
        zfile[f] <- zip.file.extract(files[f], "Rdata.zip")
      else
        zfile <- files
    }
    tab <- read.table(zfile[1], header = FALSE, as.is = TRUE)
    if (length(zfile) > 1) {
      for (i in 2:length(zfile)) 
        tab <- rbind(tab, read.table(zfile[i], header = FALSE, as.is = TRUE))
    }
    loc <- tab[which(as.character(tab[, 1]) == name), 2]
  }
  if (is.null(loc)) { 
    cat("Warning: Requested topic not found in Zelig help.  \n If you are sure the topic exists, please check \n the full documentation at http://gking.harvard.edu/zelig.  \n Now searching R-help.\n\n")
    topic <- as.name(name)
    do.call("help", list(topic, htmlhelp = TRUE))
  }
  else {
    browseURL(loc)
    invisible(name)
  }
  if (zipped) {
    for (i in 1:length(zfile))
      on.exit(unlink(zfile[i]))
  }
}
