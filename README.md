# Statistical Methods in Epidemiology package
## aka SME

## Authors: Argiris Karanikolaou(@argykar), Achilleas Stamoulopoulos(@achstam)

Package for the Lab [notes](https://github.com/argykar/Statistical-Methods-in-Epidemiology).

**Installation and importing:**

```r
# install devtools package if needed
if (!require(devtools) install.packages("devtools")
devtools::install_github("argykar/SME")
library(SME)
```

Contains functions and datasets of the class of Statistical Methods in Epidemiology.

Currently supports:
1. functions
 + `r_table()` (*@achstam*) STATA-like output for logistic and Poisson regression models.
 + `table_stata()` (*@argykar* + *@achstam*) STATA-like output for conditional logistic regression models.
 + `display()` (*@argykar*) Display dataframes in html/latex form. Uses kable functions.
2. datasets
  + wha1110
  + mwanza
  + diet
  + colon
  + melanoma
  + guy 
  + lep
  + bdendo
  + bdendo11
  + typhi
  
TODO: 
  + tests for general purpose
  + examples for functions
  
  
