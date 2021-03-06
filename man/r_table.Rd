% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_table.R
\name{r_table}
\alias{r_table}
\title{Table for reporting the results of a logistic or Poisson model}
\usage{
r_table(model)
}
\arguments{
\item{model}{glm logistic/poisson}
}
\value{

}
\description{
The output matches the one that STATA use for logistic or Poisson regression.
}
\examples{
#Not run:
#model_logistic <- glm(outcome ~ x1 + x2 + ... + xN, data, family = binomial)
#r_table(model_logistic)
#or for Poisson model
#model_poisson <- glm(outcome ~ x1 + x2 + ... + xN, data, family = poisson)
#r_table(model_poisson)
}
