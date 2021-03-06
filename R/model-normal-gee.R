#' Generalized Estimating Equation for Normal Regression
#'
#'@param formula a symbolic representation of the model to be
#'   estimated, in the form \code{y ~ x1 + x2}, where \code{y} is the
#'   dependent variable and \code{x1} and \code{x2} are the explanatory
#'   variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
#'   same dataset. (You may include more than two explanatory variables,
#'   of course.) The \code{+} symbol means ``inclusion'' not
#'   ``addition.'' You may also include interaction terms and main
#'   effects in the form \code{x1*x2} without computing them in prior
#'   steps; \code{I(x1*x2)} to include only the interaction term and
#'   exclude the main effects; and quadratic terms in the form
#'   \code{I(x1^2)}.
#'@param model the name of a statistical model to estimate.
#'   For a list of other supported models and their documentation see:
#'   \url{http://docs.zeligproject.org/articles/}.
#'@param data the name of a data frame containing the variables
#'   referenced in the formula or a list of multiply imputed data frames
#'   each having the same variable names and row numbers (created by
#'   \code{Amelia} or \code{\link{to_zelig_mi}}).
#'@param ... additional arguments passed to \code{zelig},
#'   relevant for the model to be estimated.
#'@param by a factor variable contained in \code{data}. If supplied,
#'   \code{zelig} will subset
#'   the data frame based on the levels in the \code{by} variable, and
#'   estimate a model for each subset. This can save a considerable amount of
#'   effort. You may also use \code{by} to run models using MatchIt
#'   subclasses.
#'@param cite If is set to 'TRUE' (default), the model citation will be printed
#'  to the console.
#' @param robust defaults to TRUE. If TRUE, consistent standard errors are estimated using a "sandwich"
#' estimator.
#'@param corstr defaults to "independence". It can take on the following arguments:
#'@param Independence (corstr = independence): cor(yit,yit')=0, for all t,t' with t not equal to t'.
#' It assumes that there is no correlation within the clusters and the model becomes equivalent
#'  to standard normal regression. The "working" correlation matrix is the identity matrix.
#'@param Fixed corstr = fixed): If selected, the user must define the "working" correlation
#'matrix with the R argument rather than estimating it from the model.
#'@param id: where id is a variable which identifies the clusters. The data should be sorted by
#'id and should be ordered within each cluster when appropriate
#'@param corstr: character string specifying the correlation structure: "independence",
#'"exchangeable", "ar1", "unstructured" and "userdefined"
#'@param geeglm: See geeglm in package geepack for other function arguments
#'@param Mv: defaults to 1. It specifies the number of periods of correlation and
#' only needs to be specified when \code{corstr} is stat_M_dep, non_stat_M_dep, or AR-M.
#'@param R: defaults to NULL. It specifies a user-defined correlation matrix rather than
#' estimating it from the data. The argument is used only when corstr is "fixed". The input is a TxT
#' matrix of correlations, where T is the size of the largest cluster.
#' @details
#' Additional parameters avaialable to this model include:
#' \itemize{
#'   \item \code{weights}: vector of weight values or a name of a variable in the dataset
#'   by which to weight the model. For more information see:
#'   \url{http://docs.zeligproject.org/articles/weights.html}.
#' }
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'
#' @examples
#' library(Zelig)
#' data(macro)
#' z.out <- zelig(unem ~ gdp + capmob + trade, model ="normal.gee", id = "country",
#'         data = macro, corstr = "AR-M")
#' summary(z.out)

#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_normalgee.html}
#' @import methods
#' @export Zelig-normal-gee
#' @exportClass Zelig-normal-gee
#'
#' @include model-zelig.R
#' @include model-gee.R
#' @include model-normal.R

znormalgee <- setRefClass("Zelig-normal-gee",
                           contains = c("Zelig-gee", "Zelig-normal"))

znormalgee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal-gee"
    .self$family <- "gaussian"
    .self$link <- "identity"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2011
    .self$category <- "continuous"
    .self$authors <- "Patrick Lam"
    .self$description = "General Estimating Equation for Normal Regression"
    .self$fn <- quote(geepack::geeglm)
    # JSON from parent
    .self$wrapper <- "normal.gee"
  }
)
