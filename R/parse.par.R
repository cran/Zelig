parse.par <- function(par, idx, name) {
  if (name %in% idx) 
    return(par[which(idx == name)])
  else 
    return(par[which(idx != name)])
}

