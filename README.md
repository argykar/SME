# Statistical Methods in Epidemiology package
## aka SME

## Authors: 
### Argiris Karanikolaou, Achilleas Stamoulopoulos

Package for the Lab [notes](https://github.com/argykar/Statistical-Methods-in-Epidemiology).

**Installation:**
```{r}
devtools::install_github("argykar/SME")
#import library
library(SME)
```

Contains functions and datasets of the class of Statistical Methods in Epidemiology.

Currently supports:
1. functions
 + `r_table()` (*@achstam*) Like STATA output for logistic and Poisson regression models.
 + `table_stata()` (*@argykar*) Like STATA output for conditional logistic regression models.
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
  
  TODO: tests for general purpose
  
  
