## dbailey@wustl.edu
## modeified by Ferdi  10/30/07
################################

qi.lmer <- qi.glmer <- function(object, simpar, x, x1 = NULL, y = NULL) {
        
        x <- as.data.frame(x)
        if (!is.null(x1))
          x1 <- as.data.frame(x1)


        ## original dataset
        D <- eval(getcall(object)$data, envir = parent.frame())
        fml <- eval(object@call$formula)
        parsefml <- .getRandAndFixedTerms(fml)
        
        betas <- simpar[["betas"]]
        gammas <- simpar[["gammas"]]
        alpha <- simpar[["scale"]]


        fTermsNames <- colnames(model.matrix(parsefml$fixed, data = D))
         
        fTerms <- x[,fTermsNames]
        rTerms <- list()
        for (i in 1:length(parsefml$random)){
                ## for now, intercept is always present
                tt <- terms(parsefml$random[[i]])
                attr(tt,"intercept") <- 1   
                rTermsNames <- colnames(model.matrix(tt,data=D))
                rTerms[[i]] <- x[, rTermsNames]
        }
        names(rTerms) <- names(parsefml$random)
        if (!is.null(x1)){
                fTermsNames <- colnames(model.matrix(parsefml$fixed, data = D))
                fTerms.x1 <- x1[,fTermsNames]
                rTerms.x1 <- list()
                for (i in 1:length(parsefml$random)){
                        tt <- terms(parsefml$random[[i]])
                        attr(tt,"intercept") <- 1   
                        rTermsNames <- colnames(model.matrix(tt,data=D))
                        rTerms.x1[[i]] <- x1[, rTermsNames]
                        ##rTermsNames <- colnames(model.matrix(parsefml$random[[i]],data=D))
                        ##rTerms.x1[[i]] <- x1[, rTermsNames]
                }
        names(rTerms.x1) <- names(parsefml$random)
        }
        
        ## Expected Values and Predicted Values    
        if (class(object) == "glmer"){
                family <- object@family[[1]]    ## is a list, is family name always the first element ? -Ferdi
                eta <- betas %*% t(as.matrix(fTerms))
                theta <- matrix(object@family$linkinv(eta), nrow=nrow(betas))
                mu <- eta
                ## For predicted values, add in random effects draws
                for (i in 1:length(rTerms)){
                        mu <- mu + gammas[[names(rTerms[i])]] %*% t(as.matrix(rTerms[[i]]))
                }
                mut <- matrix(object@family$linkinv(mu), nrow=nrow(betas))
                ev <- matrix(NA, nrow=nrow(theta), ncol=ncol(theta))
                pr <- matrix(NA, nrow=nrow(mut), ncol=ncol(mut))
                dimnames(ev) <- dimnames(pr) <- dimnames(theta)
                if (family == "binomial"){
                        ev <- theta
                        for (i in 1:ncol(mut)){
                                pr[,i] <- as.character(rbinom(length(mut[,i]), 1, mut[,i]))
                        }
                        if (!is.null(y)) {
                                if (NCOL(y) > 1) {
                                        y <- y[,1]
                                }
                        }
                }
                else if (family == "Gamma"){
                        ev <- theta * 1/alpha
                        for (i in 1:nrow(mut)){
                                pr[i,] <- rgamma(length(mut[i,]), shape = mut[i,], scale= 1/alpha)
                        }
                }
                else if (family == "poisson"){
                        ev <- theta
                        for (i in 1:ncol(mut)){
                                pr[,i] <- rpois(length(mut[,i]), lambda = mut[,i])
                        }
                }
        }
        else if (class(object) == "lmer"){
                ev <- betas %*% t(as.matrix(fTerms))
                mu <- ev
                ## For predicted values, add in random effects draws
                for (i in 1:length(rTerms)){
                        mu <- mu + gammas[[names(rTerms[i])]] %*% t(as.matrix(rTerms[[i]]))
                }
                pr <- mu
        }
        
        qi <- list(ev=ev, pr = pr)
        qi.name <- list(ev="Expected Values: E(Y|X)", pr="Predicted Values: Y|X")
        if (!is.null(x1)){
                if (class(object) == "glmer"){
                        theta1 <- matrix(object@family$linkinv(betas %*% t(as.matrix(fTerms.x1))),
                                         nrow = nrow(betas))
                        if (family == "Gamma")
                          ev1 <- theta1 * 1/alpha
                        else
                          ev1 <- theta1
                        qi$fd <- ev1-ev
                        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
                        if (family == "binomial") {
                                qi$rr <- ev1/ev
                                qi.name$rr <- "Risk Ratios: P(Y=1|X1)/P(Y=1|X)"
                        }
                }
                else if (class(object) == "lmer"){
                        ev1 <- betas %*% t(as.matrix(fTerms.x1))
                        qi$fd <- ev1-ev
                        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
                }
        }
        if (!is.null(y)) {
                yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
                tmp.ev <- yvar - qi$ev
                if (family == "binomial")
                  tmp.pr <- yvar - as.integer(qi$pr)
                else
                  tmp.pr <- yvar - qi$pr
                qi$att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
                qi$att.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(simpar))
                qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
                qi.name$att.pr <- "Average Treatment Effect for the Treated: Y - PR"
        }
        list(qi=qi, qi.name=qi.name)
}

    


