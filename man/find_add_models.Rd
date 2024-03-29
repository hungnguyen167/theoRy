% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_add_models.R
\name{find_add_models}
\alias{find_add_models}
\title{Find or add models to the model universe}
\usage{
find_add_models(
  ls_theory = NULL,
  causal_matrix = NULL,
  node_timing = NULL,
  user_mods,
  on_ls = TRUE,
  add_nodes = NULL,
  assert_mod_num = NULL
)
}
\arguments{
\item{ls_theory}{the input ls_theory object. Created from \code{\link{run_theoRy}}.}

\item{causal_matrix}{the input causal_matrix. Created from \code{\link{build_causal_node}}. Used only
when ls_theory=NULL.}

\item{node_timing}{the input node_timing Created from \code{\link{build_causal_node}}. Used only
when ls_theory=NULL.}

\item{user_mods}{the user's input model(s). Must be in R-like syntax.}

\item{on_ls}{whether to use ls_theory or causal matrix. Default to TRUE (use ls_theory).}

\item{add_nodes}{if the user-defined models have extra nodes/timing/types, should be provided as a list
with three named elements: nodes, types, and timing.}

\item{assert_mod_num}{model numbers to assert on user-defined models. When not provided, added models will be appended
to the end of the matrices.}
}
\value{
DAG plots of chosen DAG models from the ls_theory.
}
\description{
`find_add_models` allows users to search for a particular model within the model universe. When not found,
users can choose to add it to the universe to compare it with existing models.
}
\examples{
ls_theory <- find_add_models(ls_theory = ls_theory, on_ls = TRUE,
user_mods = c("y ~ xtest + ctr2; xtest ~ ctr3 + ctr2", # Model 1
"y ~ xtest + ctr2; xtest ~ ctr3 + ctr2", # Model 2
"y ~ xtest + ctr3 + ctr2; xtest ~ ctr3 + ctr2", # Model 3
"y ~ xtest + ctr3; xtest ~ ctr3 + ctr2; ctr4  ~ y + xtest", # Model 4
"y ~ xtest + ctr3; xtest ~ ctr3; ctr2 ~ ctr2", # Model 5
"y ~ xtest + ctr3; xtest ~ ctr2 + ctr1 + ctr3; ctr1 ~ ctr3" # Model 6),
assert_mod_num = c(1,2,3,4,5,6))

}
