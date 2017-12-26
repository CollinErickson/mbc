#' Boxplot of \code{mbc} timings.
#'
#' @param x A \code{mbc} object.
#' @param unit Unit in which the results be plotted.
#' @param log Should times be plotted on log scale?
#' @param xlab X axes label.
#' @param ylab Y axes label.
#' @param ... Passed on to boxplot.formula.
#' 
#' @export
#' @method boxplot mbc
#'
#' @importFrom graphics boxplot
#' 
#' @examples 
#' m1 <- mbc(sum(rexp(ceiling(100*rexp(1)))))
#' boxplot(m1)
#' 
#' @author Collin Erickson, Olaf Mersmann
boxplot.mbc <- function(x, unit="t", log=TRUE, xlab, ylab, ...) {
  x$time <- convert_to_unit(x$time, unit)
  timeunits <- c("ns", "us", "ms", "s", "t")
  frequnits <- c("hz", "khz", "mhz", "eps", "f")
  
  if (missing(xlab))
    xlab <- "Expression"
  if (missing(ylab)) {
    ylab <- if (log) {
      if (unit %in% timeunits)
        paste("log(time) [", unit, "]", sep="")
      else if (unit %in% frequnits)
        paste("log(frequency) [", unit, "]", sep="")
      else
        paste("log(", unit, ")", sep="")
    } else {
      if (unit %in% timeunits)
        paste("time [", unit, "]", sep="")
      else if (unit %in% frequnits)
        paste("frequency [", unit, "]", sep="")
      else if (unit == "eps")
        "evaluations per second [Hz]"
      else
        unit
    }
  }
  ll <- if (log) "y" else ""
  
  boxplot(time ~ expr, data=x, xlab=xlab, ylab=ylab, log=ll, ...)
  
  # boxplot for v1
  boxplot(V1 ~ expr, data=x, xlab=xlab, ylab="Output 1", log=ll)
}
