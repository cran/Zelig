help.zelig <- function(object) {
  # Elements of this function use functions from the Hmisc library for
  # R by Frank E Harrell Jr, distributed under the GNU GPL v.2.
  under.unix   <- !(version$os=='Microsoft Windows' ||
                    version$os=='Win32' || version$os=='mingw32')
  sys <- function (command, text = NULL) {
    cmd <- if (length(text)) 
      paste(command, text)
    else command
    if (under.unix) 
      system(cmd)
    else shell(cmd, wait = TRUE)
  }
  browser <- .Options$help.browser
  if(!length(browser)) browser <- .Options$browser
  if(!length(browser)) browser <- getOption("browser")
  url <- NULL
  if (missing(object)) 
    url <- c("http://gking.harvard.edu/zelig/docs/")
  else if (is.character(object)) { 
    z <- .libPaths()
    n <- urls <- array()
    for (i in 1:length(z))
      n[i] <- file.path(z[i], "Zelig", fsep = .Platform$file.sep)
    check <- file.exists(n)
    if (sum(check) > 1) {
      Zdir <- n[check][1]
      warning(paste("library Zelig found in two locations.  Using", 
Zdir))
    }
    else
      Zdir <- n[which(check)]
    Zdata <- file.path(Zdir, "data", fsep = .Platform$file.sep)
    files <- list.files(Zdata, pattern = "url")
    for (i in 1:length(files))
      urls[i] <- file.path(Zdata, files[i], fsep = .Platform$file.sep)
    data.path <- read.table(urls[1], header = FALSE)
    if (length(urls) > 1) {
      for (i in 2:length(urls)) { 
        tmp <- read.table(urls[i], header = FALSE)
        data.path <- rbind(data.path, tmp)
      }
    }
    url <- data.path[which(as.character(data.path[,1]) == object), 2]
  }
  else 
    stop("Please enclose the requested topic \n in quotes and try again.")
  if (is.null(url)) {
    cat("Warning: Requested topic not found in Zelig help.  \n If you are sure the topic exists, please check \n the full documentation at http://gking.harvard.edu/zelig.  \n Now searching R-help.\n\n")
    topic <- as.name(object)
    do.call("help", list(topic, htmlhelp = TRUE))
  }
  else {
    if (under.unix) {
      sys(paste(browser, as.character(url), '&'))
      invisible()
    }
    if (!under.unix) {
      browseURL(as.character(url), browser = browser)
      invisible("")
    }
  }
}  


