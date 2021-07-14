
# mlr3tuningspaces

Package website: [release](https://mlr3tuningspaces.mlr-org.com/) |
[dev](https://mlr3tuningspaces.mlr-org.com/dev/)

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3tuningspaces/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3tuningspaces/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/mlr3tuningspaces)](https://cran.r-project.org/package=mlr3tuningspaces)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

Collection of search spaces for hyperparameter tuning. Includes various
search spaces that can be directly applied to an `mlr3` learner.
Additionally, meta information about the search space can be queried.

## Installation

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3tuningspaces")
```

## Example

### Quick tuning

``` r
library(mlr3tuningspaces)

# tune learner with default search space
instance = tune(
  method = "random_search",
  task = tsk("pima"),
  learner = lts(lrn("classif.rpart")),
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10
)

# best performing hyperparameter configuration
instance$result
```

    ##    minsplit minbucket        cp learner_param_vals  x_domain classif.ce
    ## 1: 4.174471 0.5070691 -4.542023          <list[4]> <list[3]>  0.1953125

### Tuning search spaces

``` r
library("data.table")

# print keys and learners
as.data.table(mlr_tuning_spaces)
```

    ##                        key         learner n_values
    ## 1:  classif.ranger.default  classif.ranger        3
    ## 2:   classif.rpart.default   classif.rpart        3
    ## 3:     classif.svm.default     classif.svm        4
    ## 4: classif.xgboost.default classif.xgboost        9
    ## 5:     regr.ranger.default     regr.ranger        3
    ## 6:      regr.rpart.default      regr.rpart        3
    ## 7:        regr.svm.default        regr.svm        4
    ## 8:    regr.xgboost.default    regr.xgboost        9

``` r
# get tuning space and view tune token
tuning_space = lts("classif.rpart.default")
tuning_space$values
```

    ## $minsplit
    ## Tuning over:
    ## range [2, 128] (log scale)
    ## 
    ## 
    ## $minbucket
    ## Tuning over:
    ## range [1, 64] (log scale)
    ## 
    ## 
    ## $cp
    ## Tuning over:
    ## range [1e-04, 0.1] (log scale)

``` r
# get learner with tuning space
learner = tuning_space$get_learner()

# tune learner
instance = tune(
  method = "random_search",
  task = tsk("pima"),
  learner = learner,
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10)

instance$result
```

    ##    minsplit minbucket        cp learner_param_vals  x_domain classif.ce
    ## 1: 3.009338  2.506336 -8.291878          <list[4]> <list[3]>  0.2421875
