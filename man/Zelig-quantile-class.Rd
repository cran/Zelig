% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-quantile.R
\docType{class}
\name{Zelig-quantile-class}
\alias{Zelig-quantile-class}
\alias{zquantile}
\title{Quantile Regression for Continuous Dependent Variables}
\arguments{
\item{formula}{a symbolic representation of the model to be
estimated, in the form \code{y ~ x1 + x2}, where \code{y} is the
dependent variable and \code{x1} and \code{x2} are the explanatory
variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
same dataset. (You may include more than two explanatory variables,
of course.) The \code{+} symbol means ``inclusion'' not
``addition.'' You may also include interaction terms and main
effects in the form \code{x1*x2} without computing them in prior
steps; \code{I(x1*x2)} to include only the interaction term and
exclude the main effects; and quadratic terms in the form
\code{I(x1^2)}.}

\item{model}{the name of a statistical model to estimate.
For a list of other supported models and their documentation see:
\url{http://docs.zeligproject.org/articles/}.}

\item{data}{the name of a data frame containing the variables
referenced in the formula or a list of multiply imputed data frames
each having the same variable names and row numbers (created by
\code{Amelia} or \code{\link{to_zelig_mi}}).}

\item{...}{additional arguments passed to \code{zelig},
relevant for the model to be estimated.}

\item{by}{a factor variable contained in \code{data}. If supplied,
\code{zelig} will subset
the data frame based on the levels in the \code{by} variable, and
estimate a model for each subset. This can save a considerable amount of
effort. You may also use \code{by} to run models using MatchIt
subclasses.}

\item{cite}{If is set to 'TRUE' (default), the model citation will be printed
to the console.}
}
\value{
Depending on the class of model selected, \code{zelig} will return
  an object with elements including \code{coefficients}, \code{residuals},
  and \code{formula} which may be summarized using
  \code{summary(z.out)} or individually extracted using, for example,
  \code{coef(z.out)}. See
  \url{http://docs.zeligproject.org/articles/getters.html} for a list of
  functions to extract model components. You can also extract whole fitted
  model objects using \code{\link{from_zelig_model}}.
}
\description{
Quantile Regression for Continuous Dependent Variables
}
\details{
In addition to the standard inputs, \code{zelig} takes the following additional options
for quantile regression:
\itemize{
    \item \code{tau}: defaults to 0.5. Specifies the conditional quantile(s) that will be
    estimated. 0.5 corresponds to estimating the conditional median, 0.25 and 0.75 correspond
    to the conditional quartiles, etc. tau vectors with length greater than 1 are not currently
    supported. If tau is set outside of the interval [0,1], zelig returns the solution for all
    possible conditional quantiles given the data, but does not support inference on this fit
    (setx and sim will fail).
    \item \code{se}: a string value that defaults to "nid". Specifies the method by which
    the covariance matrix of coefficients is estimated during the sim stage of analysis. \code{se}
    can take the following values, which are passed to the \code{summary.rq} function from the
    \code{quantreg} package. These descriptions are copied from the \code{summary.rq} documentation.
    \itemize{
        \item \code{"iid"} which presumes that the errors are iid and computes an estimate of
        the asymptotic covariance matrix as in KB(1978).
        \item \code{"nid"} which presumes local (in tau) linearity (in x) of the the
        conditional quantile functions and computes a Huber sandwich estimate using a local
        estimate of the sparsity.
        \item \code{"ker"} which uses a kernel estimate of the sandwich as proposed by Powell(1990).
    }
    \item \code{...}: additional options passed to rq when fitting the model. See documentation for rq in the quantreg package for more information.
}
Additional parameters avaialable to this model include:
\itemize{
  \item \code{weights}: vector of weight values or a name of a variable in the dataset
  by which to weight the model. For more information see:
  \url{http://docs.zeligproject.org/articles/weights.html}.
  \item \code{bootstrap}: logical or numeric. If \code{FALSE} don't use bootstraps to
  robustly estimate uncertainty around model parameters due to sampling error.
  If an integer is supplied, the number of boostraps to run.
  For more information see:
  \url{http://docs.zeligproject.org/articles/bootstraps.html}.
}
}
\section{Methods}{

\describe{
\item{\code{zelig(formula, data, model = NULL, ..., weights = NULL, by, bootstrap = FALSE)}}{The zelig function estimates a variety of statistical models}
}}

\examples{
library(Zelig)
data(stackloss)
z.out1 <- zelig(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
model = "rq", data = stackloss,tau = 0.5)
summary(z.out1)

}
\seealso{
Vignette: \url{http://docs.zeligproject.org/articles/zelig_quantile.html}
}
