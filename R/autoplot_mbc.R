#' Autoplot method for microbenchmark objects: Prettier graphs for
#' microbenchmark using ggplot2
#'
#' Uses ggplot2 to produce a more legible graph of microbenchmark timings
#'
#' @param object A microbenchmark object
#' @param \dots Ignored
#' @param log If \code{TRUE} the time axis will be on log scale.
#' @param y_max The upper limit of the y axis (defaults to 5 percent more than
#'   the maximum value)
#' @return A ggplot2 plot
#'
#' @importFrom ggplot2 autoplot ggplot aes_string
#' @importFrom ggplot2 stat_ydensity coord_flip coord_cartesian
#' @importFrom ggplot2 scale_x_discrete scale_y_log10 scale_y_continuous
#' @export
#' @method autoplot mbc
#' @examples
#' library("ggplot2")
#'
#' tm <- mbc(rchisq(100, 0),
#'                      rchisq(100, 1),
#'                      rchisq(100, 2),
#'                      rchisq(100, 3),
#'                      rchisq(100, 5), times=1000L)
#' autoplot(tm)
#' @author Collin Erickson, Ari Friedman, Olaf Mersmann
autoplot.mbc <- function(object, ...,
                                    log=TRUE,
                                    y_max=1.05 * max(object$time)) {
  # browser()
  # 
  # y_min <- 0
  # # object$ntime <- convert_to_unit(object$time, "t")
  # melt_object <- reshape2::melt(object, id.vars=c('expr'))
  # plt <- ggplot(melt_object, ggplot2::aes(x=expr, y=value)) + facet_grid(. ~ variable, scales='free')#ggplot2::aes_string(x="expr", y="ntime"))
  # plt <- plt + coord_cartesian(ylim=c(y_min , y_max))
  # plt <- plt + stat_ydensity()
  # plt <- plt + scale_x_discrete(name="")
  # plt <- if (log) {
  #   plt + scale_y_log10(name=sprintf("Time [%s]",
  #                                    attr(object$ntime, "unit")))
  # } else {
  #   plt + scale_y_continuous(name=sprintf("Time [%s]",
  #                                         attr(object$ntime, "unit")))
  # }
  # plt <- plt + ggplot2::coord_flip()
  # print(plt)
  
  
  y_min <- 0
  object$ntime <- convert_to_unit(object$time, "t")
  plt <- ggplot(object, ggplot2::aes_string(x="expr", y="ntime"))
  plt <- plt + coord_cartesian(ylim=c(y_min , y_max))
  plt <- plt + stat_ydensity()
  plt <- plt + scale_x_discrete(name="")
  plt <- if (log) {
    plt + scale_y_log10(name=sprintf("Time [%s]",
                                              attr(object$ntime, "unit")))
  } else {
    plt + scale_y_continuous(name=sprintf("Time [%s]",
                                                   attr(object$ntime, "unit")))
  }
  plt <- plt + ggplot2::coord_flip()
  plt
  
  
  # autoplot V1
  
  y_min <- min(object$V1) - 0.03 * (max(object$V1) - min(object$V1))
  y_max <- max(object$V1) + 0.03 * (max(object$V1) - min(object$V1))
  # object$ntime <- convert_to_unit(object$time, "t")
  plt2 <- ggplot(object, ggplot2::aes_string(x="expr", y="V1"))
  plt2 <- plt2 + coord_cartesian(ylim=c(y_min , y_max))
  plt2 <- plt2 + stat_ydensity()
  plt2 <- plt2 + scale_x_discrete(name="")
  # plt <- if (log) {
  #   plt + scale_y_log10(name=sprintf("Time [%s]",
  #                                    attr(object$ntime, "unit")))
  # } else {
  #   plt + scale_y_continuous(name=sprintf("Time [%s]",
  #                                         attr(object$ntime, "unit")))
  # }
  plt2 <- plt2 + scale_y_continuous(name="V1")
  plt2 <- plt2 + ggplot2::coord_flip()
  plt2 <- plt2 +  ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                        axis.text.y=ggplot2::element_blank(),
                        axis.ticks.y=ggplot2::element_blank())
  plt2
  
  ggpubr::ggarrange(plt, plt2, ncol=2, nrow=1)
}
