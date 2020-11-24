#' Table for reporting a conditional logistic model
#'
#' Returns the results of a clogit model. The output matches the one that STATA use.
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom methods is
#' @importFrom tibble rownames_to_column
#' @import stats
#' @param model clogit
#' @param exp exponentiate for Odds Ratio or not for Coefficients
#'
#'
#' @return
#' @export
#'
#' @examples
#' #Not run:
#' #model <- clogit(outcome ~ x1 + x2 + ... + strata(group), data)
#' #table_stata(model)
table_stata <- function(model, exp = TRUE) {
  df <- as.data.frame(summary(model)$coefficients) %>%
    rownames_to_column(var = "Predictors") %>%
    bind_cols(as.data.frame(summary(model)$conf.int) %>% select(-`exp(coef)`)) %>%
    mutate(SE = `exp(coef)` * `se(coef)`,
           lower = `coef` - 1.96 * `se(coef)`,
           upper = `coef` + 1.96 * `se(coef)`) %>%
    na.omit()

  {if (exp)
    df %>% select(Predictors,
                  'OR' = `exp(coef)`,
                  SE,
                  `z`,
                  `Pr(>|z|)`,
                  `lower .95`,
                  `upper .95`) -> out
    else
      df %>% select(
        Predictors,
        'Coef' = `coef`,
        'se' = `se(coef)`,
        `z`,
        `Pr(>|z|)`,
        lower,
        upper
      ) -> out
    }
  return(out)
}

utils::globalVariables(c('exp(coef)', 'se(coef)', 'SE', 'z',
                         'upper .95', 'Predictors', 'lower .95',
                         'z value', 'Pr(>|z|)', 'lower', 'upper'))
