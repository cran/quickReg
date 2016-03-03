#' Display summary information of varibles in a data.frame
#'
#' Dispaly count, frequency or mean, standard deviation and test of normality, etc.

#' @param x A data.frame
#' @param col Column indices of variables in the dataset to display, the default columns are all the variables
#' @param normtest  A character indicating test of normality, the default method is \code{\link{shapiro.test}} when sample size no more than 5000, otherwise \code{\link[nortest]{lillie.test}}{Kolmogorov-Smirnov} is used, see package \strong{nortest} for more methods.Use "shapiro.test", "lillie.test", "ad.test", etc to specify methods.
#' @param continuous_limit A numeric defining the minimal of different value to display the variable as count and frequency
#' @param useNA Whether to include NA values in the table, see \code{\link{table}} for more details
#' @param \dots additional arguments
#' @export
#' @seealso \code{\link{display.reg}},  \code{\link{display}}
#' @examples
#' data(diabetes)
#' head(diabetes)
#' display(diabetes,1:2)
#' display(diabetes,1:11)

display.data.frame <- function(x = NULL, col = NULL, normtest = NULL, continuous_limit = 10,
    useNA = "ifany", ...) {
    data <- x
    stopifnot(is.data.frame(data))
    if (is.null(col))
        col = seq_along(1:NCOL(data))
    if (is.null(normtest)) {
      normtest<-ifelse(NROW(data)<=5000,"shapiro.test","lillie.test")
      if (NROW(data)>5000)  warning("Please install package `nortest`.", call. = FALSE)
    }
    normtest <- match.fun(normtest)
    result <- NULL

    for (i in col) {
        split_line <- "==================================================================="
        term <- names(data)[i]
        if (length(unique(data[, i])) >= continuous_limit && is.numeric(data[, 1])) {
            summary <- summary(data[, i])
            describe <- psych::describe(data[, i])
            normality <- normtest(data[, i])
            result[[term]] <- list(split_line = split_line, summary = summary, describe = describe,
                normality = normality)
        } else {
            count <- table(data[, i])
            propotion <- prop.table(count)

            if (useNA != "no") {
                if (any(is.na(data[, i]))) {
                  count_NA <- table(data[, i], useNA = useNA)
                  propotion_NA <- prop.table(count_NA)
                  result[[term]] <- list(split_line = split_line, table = rbind(count, propotion),
                    table_NA = rbind(count_NA, propotion_NA))
                } else {
                  result[[term]] <- list(split_line = split_line, table = rbind(count, propotion))

                }
            }
        }
    }
    result
}
