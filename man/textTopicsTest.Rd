% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/7_2_textTopicsAnalysis.R
\name{textTopicsTest}
\alias{textTopicsTest}
\title{This function tests the relationship between a single topic or all topics and a
variable of interest. Available tests include correlation, t-test, linear regression,
binary regression, and ridge regression. (EXPERIMENTAL - under development)}
\usage{
textTopicsTest(
  model,
  pred_var,
  group_var = NULL,
  control_vars = c(),
  test_method = "linear_regression",
  multiple_comparison = "fdr",
  load_dir = NULL
)
}
\arguments{
\item{model}{(data.frame) The model returned from textTopics().}

\item{pred_var}{(string) Variable of interest for linear or binary regression}

\item{group_var}{(string) Grouping variable for t-test}

\item{control_vars}{(list) Control variables for linear or binary regression}

\item{test_method}{(string) Choose between "correlation", "t-test", "binary_regression",
"linear_regression" or "ridge_regression"}

\item{multiple_comparison}{Method for correction of multiple tests
(e.g., "fdr", "bonferroni").}

\item{load_dir}{(string) if specified, the function returns the precomputed analysis
from the directory, otherwise leave blank}
}
\value{
Metadata and results of the test such as estimate, t-value, p-value,
and variable name.
}
\description{
This function tests the relationship between a single topic or all topics and a
variable of interest. Available tests include correlation, t-test, linear regression,
binary regression, and ridge regression. (EXPERIMENTAL - under development)
}
