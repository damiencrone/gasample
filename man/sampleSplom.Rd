% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleSplom.R
\name{sampleSplom}
\alias{sampleSplom}
\title{Function to produce a scatterplot matrix comparing a sample of items to the
population from which they were drawn.}
\usage{
sampleSplom(items, dat, label_vec = NULL, selected_col = "black",
  nonselected_col = "gray", mar = c(3, 3, 0.5, 0.5), mgp = c(2, 0.5,
  0), nonselected_alpha = 0.1, xlim = NULL, ylim = NULL,
  text_size = NULL, include_nonsampled = TRUE)
}
\arguments{
\item{items}{a character vector of items used to index dat}

\item{dat}{a matrix or dataframe containing describing each item in the population on the dimensions to be plotted}

\item{label_vec}{optional character vector of variable labels}

\item{selected_col}{colour for plotting selected items}

\item{nonselected_col}{colour for plotting nonselected items}

\item{mar}{vector containing margin parameters passed to par()}

\item{mgp}{vector containing axis label locations passed to par()}

\item{nonselected_alpha}{transparency for scatterplot points for nonselected items}

\item{xlim}{an xlim vector or list of xlim vectors}

\item{ylim}{a ylim vector or list of ylim vectors}

\item{include_nonsampled}{logical denoting whether to plot nonsampled items}
}
\description{
Function to produce a scatterplot matrix comparing a sample of items to the
population from which they were drawn.
}
