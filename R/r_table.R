#' Table for reporting the results of a logistic or Poisson model
#'
#' The output matches the one that STATA use for logistic or Poisson regression.
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom methods is
#' @importFrom tibble rownames_to_column
#' @import stats
#' @param model glm logistic/poisson
#'
#' @return
#' @export
#'
#' @examples
#' #Not run:
#' #model_logistic <- glm(outcome ~ x1 + x2 + ... + xN, data, family = binomial)
#' #r_table(model_logistic)
#' #or for Poisson model
#' #model_poisson <- glm(outcome ~ x1 + x2 + ... + xN, data, family = poisson)
#' #r_table(model_poisson)
r_table <- function(model) {
  if (is(model, "glm") == TRUE) {
    if (family(model)$family == "poisson") {
      modification <-
        model %>%
        summary() %>%
        coefficients() %>%
        as.data.frame() %>%
        slice(-1) %>%
        rownames_to_column(var = "Predictors") %>%
        mutate(IRR = exp(Estimate),
               Std_Err = IRR * `Std. Error`,
               pval = sprintf("%1.3f", `Pr(>|z|)`),
               pvalue = ifelse(pval < 0.001, '<0.001', pval)) %>%
        select(Predictors,
               IRR, Std_Err,
               "z_value" = `z value`,
               "P-Value" = pvalue)

      if (nrow(modification) > 1) {
        final_table <- bind_cols(modification,
                                 LL = exp(confint.default(model)[-1, ])[, 1],
                                 UL = exp(confint.default(model)[-1, ])[, 2])
      } else {
        final_table <- bind_cols(modification,
                                 LL = exp(confint.default(model)[-1, ])[1],
                                 UL = exp(confint.default(model)[-1, ])[2])
      }
    } else if (family(model)$family == "binomial") {
      modification <-
        model %>%
        summary() %>%
        coefficients() %>%
        as.data.frame() %>%
        slice(-1) %>%
        rownames_to_column(var = "Predictors") %>%
        mutate(OR = exp(Estimate),
               Std_Err = OR * `Std. Error`,
               pval = sprintf("%1.3f", `Pr(>|z|)`),
               pvalue = ifelse(pval < 0.001, '<0.001', pval)) %>%
        select(Predictors,
               OR, Std_Err,
               "z_value" = `z value`,
               "P-Value" = pvalue)

      if (nrow(modification) > 1) {
        final_table <- bind_cols(modification,
                                 LL = exp(confint.default(model)[-1, ])[, 1],
                                 UL = exp(confint.default(model)[-1, ])[, 2])
      } else {
        final_table <- bind_cols(modification,
                                 LL = exp(confint.default(model)[-1, ])[1],
                                 UL = exp(confint.default(model)[-1, ])[2])
      }
    } else {
      stop("Family of the model is not Poisson or Binomial")
    }
  } else {
    stop("Object is not of class glm")
  }
  return(final_table)
}


utils::globalVariables(c('Estimate', 'IRR', 'Std. Error', 'Pr(>|z|)',
                         'pval', 'Predictors', 'Std_Err', 'z value',
                         'pvalue', 'OR'))
