\name{rocplot}

\alias{rocplot}
\alias{roc}
\alias{ROC}
\alias{ROCplot}

\title{Receiver Operator Characteristic Plots}

\description{
The \code{rocplot} command generates a receiver operator
characteristic plot to compare the in-sample (default) or
out-of-sample fit for two logit or probit regressions.  
}

\usage{
rocplot(y1, y2, fitted1, fitted2, cutoff = seq(from=0, to=1, length=100), 
        lty1 = "solid", lty2 = "dashed", lwd1 = par("lwd"), lwd2 = par("lwd"),
        col1 = par("col"), col2 = par("col"), main, xlab, ylab, ...)
}

\arguments{
\item{y1}{Response variable for the first model.  }
\item{y2}{Response variable for the second model. }
\item{fitted1}{Fitted values for the first model.  These values
  may represent either the in-sample or out-of-sample fitted values. }
\item{fitted2}{Fitted values for the second model. } 
\item{cutoff}{A vector of cut-off values between 0 and 1, at
  which to evaluate the proportion of 0s and 1s correctly predicted by
  the first and second model.  By default, this is 100 increments
  between 0 and 1, inclusive.  }
\item{lty1, lty2}{The line type for the first model ({\tt
    lty1}) and the second model (\code{lty2}), defaulting to solid and
  dashed, respectively.    }
\item{lwd1, lwd2}{The width of the line for the first
  model (\code{lwd1}) and the second model (\code{lwd2}), defaulting to
  1 for both.  }
\item{col1, col2}{The colors of the line for the first
  model (\code{col1}) and the second model (\code{col2}), defaulting to
  black for both. }
\item{main}{a title for the plot.  Defaults to "Roc Curve".}
\item{xlab}{a label for the x-axis.  Defaults to "Proportion of 1's 
Correctly Predicted".}
\item{ylab}{a label for the y-axis.  Defaults to Proportion of 0's 
Correctly Predicted".}
\item{\dots}{Additional parameters passed to plot, including
  \code{xlab}, \code{ylab}, and \code{main}.  }
}

\value{
For two logit or probit models, \code{rocplot} returns an on-screen
window with the desired ROC plot.  
}

\example{
data(turnout)
z.out1 <- zelig(vote ~ race + educate + age, model = "logit", 
                data = turnout)
z.out2 <- zelig(vote ~ race + educate, model = "logit", 
                data = turnout)
rocplot(z.out1$y, z.out2$y, fitted(z.out1), fitted(z.out2))
}

\seealso{ The full Zelig manual (available at
  \url{http://gking.harvard.edu/zelig}), \code{plot}, \code{lines}.  }

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>
}

\keyword{aplot}
