% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-ivreg.R
\docType{class}
\name{Zelig-ivreg-class}
\alias{Zelig-ivreg-class}
\alias{zivreg}
\title{Instrumental-Variable Regression}
\source{
\code{ivreg} is from Christian Kleiber and Achim Zeileis (2008). Applied
Econometrics with R. New York: Springer-Verlag. ISBN 978-0-387-77316-2. URL
\url{https://CRAN.R-project.org/package=AER}
}
\arguments{
\item{model}{the name of a statistical model to estimate.
For a list of other supported models and their documentation see:
\url{http://docs.zeligproject.org/articles/}.}

\item{data}{the name of a data frame containing the variables
referenced in the formula or a list of multiply imputed data frames
each having the same variable names and row numbers (created by
\code{Amelia} or \code{\link{to_zelig_mi}}).}

\item{by}{a factor variable contained in \code{data}. If supplied,
\code{zelig} will subset
the data frame based on the levels in the \code{by} variable, and
estimate a model for each subset. This can save a considerable amount of
effort. You may also use \code{by} to run models using MatchIt
subclasses.}

\item{cite}{If is set to 'TRUE' (default), the model citation will be printed
to the console.}

\item{formula}{specification(s) of the regression relationship}

\item{instruments}{the instruments. Either \code{instruments} is missing and
formula has three parts as in \code{y ~ x1 + x2 | z1 + z2 + z3} (recommended) or
formula is \code{y ~ x1 + x2} and instruments is a one-sided formula
\code{~ z1 + z2 + z3}. Using \code{instruments} is not recommended with \code{zelig}.}

\item{model, x, y}{logicals. If \code{TRUE} the corresponding components of the fit
(the model frame, the model matrices , the response) are returned.}

\item{...}{further arguments passed to methods. See also \code{\link{zelig}}.}
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
Instrumental-Variable Regression
}
\details{
Additional parameters avaialable to many models include:
\itemize{
\item weights: vector of weight values or a name of a variable in the dataset
by which to weight the model. For more information see:
\url{http://docs.zeligproject.org/articles/weights.html}.
\item bootstrap: logical or numeric. If \code{FALSE} don't use bootstraps to
robustly estimate uncertainty around model parameters due to sampling error.
If an integer is supplied, the number of boostraps to run.
For more information see:
\url{http://docs.zeligproject.org/articles/bootstraps.html}.
}

Regressors and instruments for \code{ivreg} are most easily specified in
a formula with two parts on the right-hand side, e.g.,
\code{y ~ x1 + x2 | z1 + z2 + z3}, where \code{x1} and \code{x2} are the regressors and
\code{z1}, \code{z2}, and \code{z3} are the instruments. Note that exogenous regressors
have to be included as instruments for themselves. For example, if there is
one exogenous regressor \code{ex} and one endogenous regressor \code{en} with
instrument \verb{in}, the appropriate formula would be \verb{y ~ ex + en | ex + in}.
Equivalently, this can be specified as \verb{y ~ ex + en | . - en + in}, i.e.,
by providing an update formula with a \code{.} in the second part of the
formula. The latter is typically more convenient, if there is a large
number of exogenous regressors.
}
\section{Methods}{

\describe{
\item{\code{zelig(formula, data, model = NULL, ..., weights = NULL, by, bootstrap = FALSE)}}{The zelig function estimates a variety of statistical models}
}}

\examples{
library(Zelig)
library(dplyr) # for the pipe operator \%>\%
# load and transform data
data("CigarettesSW")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
# log second stage independent variables, as logging internally for ivreg is
# not currently supported
CigarettesSW$log_rprice <- log(CigarettesSW$rprice)
CigarettesSW$log_rincome <- log(CigarettesSW$rincome)
z.out1 <- zelig(log(packs) ~ log_rprice + log_rincome |
log_rincome + tdiff + I(tax/cpi),data = CigarettesSW, subset = year == "1995",model = "ivreg")
summary(z.out1)
library(Zelig)
library(AER) # for sandwich vcov
library(dplyr) # for the pipe operator \%>\%

# load and transform data
data("CigarettesSW")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

# log second stage independent variables, as logging internally for ivreg is
# not currently supported
CigarettesSW$log_rprice <- log(CigarettesSW$rprice)
CigarettesSW$log_rincome <- log(CigarettesSW$rincome)

# estimate model
z.out1 <- zelig(log(packs) ~ log_rprice + log_rincome |
                    log_rincome + tdiff + I(tax/cpi),
                    data = CigarettesSW,
                    model = "ivreg")
summary(z.out1)

}
\seealso{
Vignette: \url{http://docs.zeligproject.org/articles/zelig_ivreg.html}
Fit instrumental-variable regression by two-stage least squares. This is
equivalent to direct instrumental-variables estimation when the number of
instruments is equal to the number of predictors.

\code{\link{zelig}},
Greene, W. H. (1993) \emph{Econometric Analysis}, 2nd ed., Macmillan.
}
