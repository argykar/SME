#' Display the results of the data frame in a nice table format
#'
#' It's a simple wrapper function of the package `kableExtra`.
#' @importFrom kableExtra kbl kable_styling
#' @importFrom magrittr %>%
#' @param x data.frame
#' @param digits numeric
#'
#' @return
#' @export
#'
#' @examples
#' #Not run:
#' #x = data.frame(a = seq(0, 10, 1), b = seq(1, 11, 1))
#' #display(x)
display <- function(x, digits = 3) {
  x %>%
    kbl(digits = digits) %>%
    kable_styling()
}
