rocplot <- function(y1, y2, fitted1, fitted2,
                    cutoff = seq(from=0, to=1, length=100), lty1="solid",
                    lty2="dashed", lwd1=par("lwd"), lwd2=par("lwd"),
                    col1=par("col"), col2=par("col"), main="ROC Curve",
                    xlab = "Proportion of 1's Correctly Predicted",
                    ylab="Proportion of 0's Correctly Predicted", ...) {
  op <- par(no.readonly = TRUE)
  par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
  ones <- zeros <- matrix(NA, nrow = length(cutoff), ncol = 2)
  for (i in 1:length(cutoff)) {
    ones[i,1] <- mean(fitted1[y1==1] >= cutoff[i]) 
    ones[i,2] <- mean(fitted2[y2==1] >= cutoff[i])
    zeros[i,1] <- mean(fitted1[y1==0] < cutoff[i])
    zeros[i,2] <- mean(fitted2[y2==0] < cutoff[i])
  }
  plot(0:1, 0:1, type = "n", xaxs = "i", yaxs = "i",
       main=main, xlab=xlab, ylab=ylab, ...)
  lines(cbind(ones[,1], zeros[,1]), lty = lty1, lwd = lwd1, col=col1)
  lines(cbind(ones[,2], zeros[,2]), lty = lty2, lwd = lwd2, col=col2)
  abline(1, -1, lty = "dotted")
  par(op)
}
















