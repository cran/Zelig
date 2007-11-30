zelig3ls.mixed <- zelig3gamma.mixed <- zelig3poisson.mixed <- zelig3probit.mixed <- zelig3logit.mixed <- function (res, fcall = NULL,zcall=NULL) {
        tt <- attr(res@frame, "terms")
        attr(tt, "intercept") <- 1
        res@terms <- tt
        return (res)
}
