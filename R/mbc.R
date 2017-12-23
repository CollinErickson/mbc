#' @useDynLib mbc do_microtiming do_microtiming_precision do_microtiming2 do_microtiming3
{}

#' Sub-millisecond accurate timing of expression evaluation with output summary and comparison.
#'
#' \code{mbc} is a modified version of the function \code{microbenchmark}
#' from the microbenchmark R package. In addition to showing the run
#' time summary, it also shows a summary of the output returned by
#' the code.
#'
#' This function is only meant for micro-benchmarking small pieces of
#' source code and to compare their relative performance
#' characteristics. You should generally avoid benchmarking larger
#' chunks of your code using this function. Instead, try using the R
#' profiler to detect hot spots and consider rewriting them in C/C++
#' or FORTRAN.
#'
#' The \code{control} list can contain the following entries:
#' \describe{
#' \item{order}{the order in which the expressions are evaluated.
#'   \dQuote{random} (the default) randomizes the execution order,
#'   \dQuote{inorder} executes each expression in order and
#'   \dQuote{block} executes all repetitions of each expression
#'     as one block.}
#' \item{warmup}{the number of warm-up iterations performed before
#'   the actual benchmark. These are used to estimate the timing
#'   overhead as well as spinning up the processor from any sleep
#'   or idle states it might be in. The default value is 2.}
#' }
#'
#' @note Depending on the underlying operating system, different
#' methods are used for timing. On Windows the
#' \code{QueryPerformanceCounter} interface is used to measure the
#' time passed. For Linux the \code{clock_gettime} API is used and on
#' Solaris the \code{gethrtime} function. Finally on MacOS X the,
#' undocumented, \code{mach_absolute_time} function is used to avoid
#' a dependency on the CoreServices Framework.
#'
#' Before evaluating each expression \code{times} times, the overhead
#' of calling the timing functions and the C function call overhead
#' are estimated. This estimated overhead is subtracted from each
#' measured evaluation time. Should the resulting timing be negative,
#' a warning is thrown and the respective value is replaced by
#' \code{0}. If the timing is zero, a warning is raised.
#' Should all evaluations result in one of the two error conditions described above, an error is raised.
#'
#' One platform on which the clock resolution is known to be too low to measure short runtimes with the required precision is 
#' Oracle\if{html}{\out{&reg;}}\if{latex}{\out{\textregistered\ }}\if{text}{(R)}
#' Solaris 
#' on some 
#' SPARC\if{html}{\out{&reg;}}\if{latex}{\out{\textregistered\ }}\if{text}{(R)}
#' hardware.
#' Reports of other platforms with similar problems are welcome.
#' Please contact the package maintainer.
#'
#' @importFrom stats aggregate fivenum lm
#'
#' @param ... Expressions to benchmark.
#' @param list  List of unevaluated expression to benchmark.
#' @param input Code chunk enclosed in braces that is evaluated before each evaluation.
#' Its output is kept in the environment and thus can be called in the expressions.
#' @param times Number of times to evaluate the expression.
#' @param check Function to check if the expressions are equal. By default \code{NULL} which omits the check.
#' @param control List of control arguments. See Details.
#' @param unit Default unit used in \code{summary} and \code{print}.
#'
#' @return Object of class \sQuote{microbenchmark}, a data frame with
#' columns \code{expr} and \code{time}. \code{expr} contains the
#' deparsed expression as passed to \code{microbenchmark} or the name
#' of the argument if the expression was passed as a named
#' argument. \code{time} is the measured execution time of the
#' expression in nanoseconds. The order of the observations in the
#' data frame is the order in which they were executed.
#'
#' @seealso \code{\link{print.microbenchmark}} to display and
#' \code{\link{boxplot.microbenchmark}} or
#' \code{\link{autoplot.microbenchmark}} to plot the results.
#'
#' @examples
#' ## Measure the time it takes to dispatch a simple function call
#' ## compared to simply evaluating the constant \code{NULL}
#' f <- function() NULL
#' res <- microbenchmark(NULL, f(), times=1000L)
#'
#' ## Print results:
#' print(res)
#'
#' ## Plot results:
#' boxplot(res)
#'
#' ## Pretty plot:
#' if (require("ggplot2")) {
#'   autoplot(res)
#' }
#'
#' ## Example check usage
#' my_check <- function(values) {
#'   all(sapply(values[-1], function(x) identical(values[[1]], x)))
#' }
#'
#' f <- function(a, b)
#'   2 + 2
#'
#' a <- 2
#' ## Check passes
#' microbenchmark(2 + 2, 2 + a, f(2, a), f(2, 2), check=my_check)
#' \dontrun{
#' a <- 3
#' ## Check fails
#' microbenchmark(2 + 2, 2 + a, f(2, a), f(2, 2), check=my_check)
#' }
#' 
#' ## Compute the mean of a sample of random variables with exponential distribution
#' mbc(mean(rexp(100)))
#' 
#' ## Compare the mean and median of exponential random samples
#' mbc(mean(rexp(100)))
#' @export
#' @author Collin Erickson, Olaf Mersmann
mbc <- function(..., list=NULL,
                           times=100L,
                           input,
                           unit,
                           check=NULL,
                           control=list()) {
  stopifnot(times == as.integer(times))
  if (!missing(unit))
    stopifnot(is.character("unit"), length(unit) == 1L)

  control[["warmup"]] <- coalesce(control[["warmup"]], 2^18L)
  # control[["order"]] <- coalesce(control[["order"]], "random")
  control[["order"]] <- coalesce(control[["order"]], "inorder") # Making default inorder in case input is given

  stopifnot(as.integer(control$warmup) == control$warmup)

  exprs <- c(as.list(match.call(expand.dots = FALSE)$`...`), list)
  nm <- names(exprs)
  exprnm <- sapply(exprs, function(e) paste(deparse(e), collapse=" "))
  if (is.null(nm))
    nm <- exprnm
  else
    nm[nm == ""] <- exprnm[nm == ""]
  names(exprs) <- nm

  if (!is.null(check)) {
    ## Evaluate values in parent environment
    values <- lapply(exprs, eval, parent.frame())
    ok <- check(values)

    if (!isTRUE(ok)) {
      stop("Input expressions are not equivalent.", call. = FALSE)
    }
  }

  ## GC first
  gc(FALSE)
  

  # o <- if (control$order == "random")
  #   sample(rep(seq_along(exprs), times=times))
  # else if (control$order == "inorder")
  #   rep(seq_along(exprs), times=times)
  # else if (control$order == "block")
  #   rep(seq_along(exprs), each=times)
  # else
  #   stop("Unknown ordering. Must be one of 'random', 'inorder' or 'block'.")
  # exprs <- exprs[o]
  
  # New version to give ordering too
  oo <- if (control$order == "random")
    sample(0:(length(exprs) * times - 1))
  else if (control$order == "inorder")
    0:(length(exprs) * times - 1)
  else if (control$order == "block")
    stop("Block not implemented, use 'random', or 'inorder'")
  else
    stop("Unknown ordering. Must be one of 'random', 'inorder' or 'block'.")
  o <- oo %% length(exprs) + 1
  env_indices <- oo %/% length(exprs)
  exprs <- exprs[o]

  
  ## Adding for mbc: eval input to create environment
  # browser()
  if (!missing(input)) {
    if (control$order != "inorder") {stop("Run order must be inorder when input is given")}
    input_expr <- match.call(expand.dots = FALSE)$`input`
    
    # Evaluate expression times times and create separate environments to pass to dmt3
    envs <- lapply(1:times, function(i) {tenv <- new.env(parent = parent.frame()); eval(input_expr, tenv); tenv})
    res <- .Call(do_microtiming3, exprs, envs, as.integer(control$warmup), env_indices)
  } else {
    # Original call
    res <- .Call(do_microtiming2, exprs, parent.frame(), as.integer(control$warmup))
  }

  ## Sanity check. Fail as early as possible if the results are
  ## rubbish.
  if (all(is.na(res)))
    .all_na_stop()

  # browser()
  #as.data.frame(do.call(rbind, res[[2]]))
  #res <- data.frame(expr = factor(nm[o], levels = nm), time=res)
  res <- data.frame(expr = factor(nm[o], levels = nm), time=res[[1]], as.data.frame(do.call(rbind, res[[2]])))
  class(res) <- c("mbc", class(res))
  if (!missing(unit))
    attr(res, "unit") <- unit
  res
}
