#' Produce Parallel Coordinate Plot Imitating ggparcoord
#'
#' This function produce parallel coordinate plot pretty much like
#' GGally::ggparcoord. It supports color and jitter, and some
#' straight forward modifications from ggplot2. But some functionalities
#' are under work.
#'
#' @param data the data.frame to plot
#' @param columns a vector of variables (either names or indices) to be axes in the plot
#' @param groupColumn a single variable to group (color) by
#' @param jitter a single logical value, jitter the points of the selected varaible(s) or not
#' @param jittervariables varibles that will be jittered, should be a subset of columns, both indices or names;
#' if unspecified, all of the variables will be jittered
#' @param jitterratio the ratio of the maximum random ajustment devided by the range of selected variables (after scaling)
#' @param mapping NOT AVAILABLE NOW, aes string to pass to ggplot2 object
#' @param ... NOT AVAILABLE NOW, passed on directly to all of the ggplot2 commands
#'
#' @export myparallel
#' @return returns a ggplot2 object that can be plotted directly or used as base layers for additional modification
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr gather
#' @importFrom stats runif
#' @import ggplot2
#'
#' @examples
#' # use sample of the diamonds data for illustrative purposes
#' data(diamonds, package="ggplot2")
#' diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 100), ]
#'
#' # default use
#' myparallel(data = diamonds.samp, columns = c(1, 5:7))
#'
#' # add color
#' myparallel(data = diamonds.samp, columns = c(1, 5:7), groupColumn = "cut")
#'
#' # jitter and title
#' myparallel(data = diamonds.samp, columns = c(1, 5:7), groupColumn = "cut",
#' jitter = TRUE, jittervariables = "table") +
#' ggplot2::ggtitle("Hello Parallel")


myparallel <- function(data, columns = 1:ncol(data), groupColumn = NULL,
                       jitter = FALSE, jittervariables = NULL, jitterratio = 0.1,
                       mapping = NULL, ...) {
  # check input here, coerce variables(don't do this now)
  if (!is.data.frame(data)) stop("data needs to be a data.frame")
  if (!(is.numeric(columns)|is.character(columns))) stop("colums should be interger positions or strings for names")
  if (!is.logical(jitter)) stop("jitter should be a logic")
  if (!is.null(jittervariables)){
    if (jitter == FALSE) stop("jitter turned off, but provided jittervariables")
    if (!all(names(data[jittervariables]) %in% names(data[columns]))) stop("jittervariables should be a subset of columns")
  }
  # process data

  n <- nrow(data)
  if (n <= 0) stop("empty data set")
  data2 <- data.frame(scale(data[columns]))


  # for jitter

  # kind of lazy, the random ajustment are given to the jitter variables as a whole,
  # instead of one by one, but this is reasonable because they are already scaled,
  # besides, this actually keeps the slope of the lines unchanged when you jitter two or more variables, extra gain!!!
  if (jitter == T){
    if (!is.null(jittervariables)) {
      jittervariables <- names(data[jittervariables])
      jitterrangeminmax <- range(data2[jittervariables])
      jitterrange <- jitterrangeminmax[2] - jitterrangeminmax[1]
      data2[jittervariables] <- data2[jittervariables] + runif(n,
                                                               min = -jitterratio*jitterrange,
                                                               max = jitterratio*jitterrange)
    } else {
      # jitter all the variables
      jitterrangeminmax <- range(data2)
      jitterrange <- jitterrangeminmax[2] - jitterrangeminmax[1]
      data2 <- data2 + stats::runif(n,
                                    min = -jitterratio*jitterrange,
                                    max = jitterratio*jitterrange)
    }
  }

  # for color
  if (is.null(groupColumn)) {
    color = NULL
    data3 <- data.frame(data2,
                        group = 1:n)
  } else {
    color = names(data[groupColumn])

    data3 <- data.frame(data2,
                        group = 1:n,
                        color = data[groupColumn]) # the "color" here doesn't change the name
  }

  # continue
  data4 <- data3 %>%
    tidyr::gather(key = "originalvariables", value = "scaledvalue", 1:length(columns), factor_key = T)

  data5 <- data.frame(data4, variable= as.numeric(data4$originalvariables))

  data5 %>%
    ggplot() +
    geom_line(aes_string(x = "variable", y = "scaledvalue",
                         group = "group", color = color)) +
    scale_x_continuous(breaks = 1:length(columns), labels = unique(data5$originalvariables))

}
