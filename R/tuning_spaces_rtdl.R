#' @title Deep Learning Tuning Spaces from Yandex's RTDL
#'
#' @name mlr_tuning_spaces_rtdl
#'
#' @description
#' Tuning spaces for deep neural network architectures from the `r cite_bib("gorishniy2021revisting")` article.
#' 
#' When the article suggests multiple ranges for a given hyperparameter, these tuning spaces choose the widest range.
#'
#' @source
#' `r format_bib("gorishniy2021revisting")`
#'
#' @aliases
#' mlr_tuning_spaces_classif.ft_transformer.rtdl
#'
#' @section FT-Transformer tuning space
#' `r rd_info(lts("classif.ft_transformer.rtdl"))`
#'
#' @include mlr_tuning_spaces.R
NULL

# mlp
vals = c(
  n_layers = to_tune(1, 16),
  neurons = to_tune(1, 1024),
  p = to_tune(0, 0.5),
  opt.lr = to_tune(1e-5, 1e-2, logscale = TRUE),
  opt.weight_decay = to_tune(1e-6, 1e-3, logscale = TRUE),
  epochs = to_tune(upper = 100L, internal = TRUE)
)

add_tuning_space(
  id = "classif.mlp.rtdl",
  values = vals,
  tags = c("gorishniy2021", "classification"),
  learner = "classif.mlp",
  package = "mlr3torch",
  label = "Classification MLP with RTDL"
)

add_tuning_space(
  id = "regr.mlp.rtdl",
  values = vals,
  tags = c("gorishniy2021", "regression"),
  learner = "regr.mlp",
  package = "mlr3torch",
  label = "Regression MLP with RTDL"
)

# resnet
vals = list(
  n_blocks = to_tune(1, 16),
  d_block = to_tune(64, 1024),
  d_hidden_multiplier = to_tune(1, 4),
  dropout1 = to_tune(0, 0.5),
  dropout2 = to_tune(0, 0.5),
  opt.lr = to_tune(1e-5, 1e-2, logscale = TRUE),
  opt.weight_decay = to_tune(1e-6, 1e-3, logscale = TRUE),
  epochs = to_tune(upper = 100L, internal = TRUE)
)

add_tuning_space(
  id = "classif.tab_resnet.rtdl",
  values = vals,
  tags = c("gorishniy2021", "classification"),
  learner = "classif.tab_resnet",
  package = "mlr3torch",
  label = "Classification Tabular ResNet with RTDL"
)

add_tuning_space(
  id = "regr.tab_resnet.rtdl",
  values = vals,
  tags = c("gorishniy2021", "regression"),
  learner = "regr.tab_resnet",
  package = "mlr3torch",
  label = "Regression Tabular ResNet with RTDL"
)

# ft_transformer
vals = list(
  n_blocks = to_tune(1, 6),
  d_token = to_tune(64, 512),
  residual_dropout = to_tune(0, 0.2),
  attention_dropout = to_tune(0, 0.5),
  ffn_dropout = to_tune(0, 0.5),
  ffn_d_hidden_multiplier = to_tune(2 / 3, 8 / 3),
  opt.lr = to_tune(1e-5, 1e-3, logscale = TRUE),
  opt.weight_decay = to_tune(1e-6, 1e-3, logscale = TRUE),
  epochs = to_tune(upper = 100L, internal = TRUE)
)

add_tuning_space(
  id = "classif.ft_transformer.rtdl",
  values = vals,
  tags = c("gorishniy2021", "classification"),
  learner = "classif.ft_transformer",
  package = "mlr3torch",
  label = "Classification FT-Transformer with RTDL"
)

add_tuning_space(
  id = "regr.ft_transformer.rtdl",
  values = vals,
  tags = c("gorishniy2021", "regression"),
  learner = "regr.ft_transformer",
  package = "mlr3torch",
  label = "Regression FT-Transformer with RTDL"
)