param.lmer <- param.glmer <- function(object, num, bootstrap = FALSE) {

	if (!bootstrap) {
                object     <- selectMethod("summary", "lmer") (object)
                #print(class(object))
                fixed      <- fixef(object)
                vcov.fix   <- vcov(object)
                betasF <- NULL
                if (length(fixed) > 0){
                        betasF <- mvrnorm(num, fixed, vcov.fix)
                }
                scale  <- attr(VarCorr(object), "sc")
                vars   <- object@bVar
                gammas <- NULL
                n.G    <- length(vars)
                
                for (m in 1:n.G){
                        V.beta <- VarCorr(object)[[m]]
                        vars.m <- vars[[m]]
                        J <- dim(vars.m)[1]
                        gammas[[m]] <- mvrnorm(num, rep(0, J), V.beta)
                }
                names(gammas) <- names(vars)
                betas <- betasF
                scale <- object@sigma
                
        } else {
                object <- summary(object)
                fixed  <- fixef(object)
                scale  <- attr (VarCorr(object), "sc")
                betasF <- fixed
                vars   <- object@bVar
                gammas <- NULL
                n.G    <- length(vars)
                for (m in 1:n.G){
                        V.beta <- VarCorr(object)[[m]]
                        gammas[[m]] <- mvrnorm(1, 0, V.beta)
                }
                names(gammas)<- names(vars)
                betas  <- betasF
                scale  <- object@sigma
        }
        res <- list(betas=betas, gammas=gammas, scale=scale)
        res
}
