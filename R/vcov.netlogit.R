vcov.logit.net <- function(object, ...)
{
    so <- summary.glm(object, correlation=FALSE, ...)
    so$dispersion * so$cov.unscaled
}
