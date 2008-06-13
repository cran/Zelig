param.lmer <- param.glmer <- function(object, num, bootstrap = FALSE) {

       fixed <- fixef(object)
       vars <- object@bVar
       scale <- attr(VarCorr(object), "sc")
       gammas <- NULL
       n.G <- length(vars)

	     if (!bootstrap) {
                object <- selectMethod("summary", "lmer") (object)
                ## print(class(object))
                ## *Sample* fixed effects
                betasF <- NULL
                vcov.fix <- vcov(object)
                if (length(fixed) > 0){
                        betasF <- mvrnorm(num, fixed, vcov.fix)
                }
                ## *Sample* random effects
                for (m in 1:n.G){
                        V.beta <- VarCorr(object)[[m]]
                        vars.m <- vars[[m]]
                        J <- dim(vars.m)[1]
                        gammas[[m]] <- mvrnorm(num, rep(0, J), V.beta)
                }
        } else {
                object <- summary(object)
                ## *Set* fixed effects
                betasF <- fixed
                ## *Sample* random effects
                for (m in 1:n.G){
                        V.beta <- VarCorr(object)[[m]]
                        gammas[[m]] <- mvrnorm(1, 0, V.beta)
                }
        }

        names(gammas) <- names(vars)
        betas <- betasF
        scale <- object@sigma

        ## Return
        list(betas=betas, gammas=gammas, scale=scale)
}
