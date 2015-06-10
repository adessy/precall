# Copyright © 2013-2015 Université catholique de Louvain, Belgium - UCL
# All rights reserved.
#
# This file is part of the precall package.
#
# The precall package has been developed by Adrien Dessy [Machine Learning
# Group (MLG) - Institute of Information and Communication Technologies,
# Electronics and Applied Mathematics (ICTEAM)] for the Université catholique de
# Louvain (UCL). The precall package enables to plot precision-recall curves and
# to compute the Area Under Precision-Recall curves.
#
# The precall package is distributed under the terms of the MIT License (MIT).
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is furnished
# to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


#' @importFrom magrittr "%>%"
NULL


#' Plot precision-recall curves
#'
#' \code{plot_precall} creates a ggplot object that represents precision-recall
#' curves corresponding to several rankings.
#'
#' @param scores A matrix or vector of numeric values that specifies the rankings
#' @param labels A vector of logical values that encodes the POSITIVE/NEGATIVE status of data
#' @param print_friendly A boolean
#' @param use_color A boolean
#' @param use_linetype A boolean
#' @param axis_label_size A numeric
#' @param legend_title_size A numeric
#' @param legend_text_size A numeric
#' @param linewidth A numeric
#'
#' @details
#'
#' The \code{labels} encode the POSITIVE/NEGATIVE (\code{TRUE}/\code{FALSE}) status of data
#' while each column of \code{scores} provides scores corresponding to a ranking of those data:
#' a smaller score means a smaller rank.
#'
#' A precision-recall curve is plotted for each column of the \code{scores}. The interpolation
#' between points is done according to the principle of partial acceptance.
#'
#' \code{print_friendly} enables to produce print-friendly plots.
#' If set to \code{TRUE}, the function yields a black and white plot (background and lines)
#' with a different line type for each curve.
#' \code{use_color} and \code{use_linetype}, if present, specifies if the curved should be colored or
#' drawn with different line types.
#' These parameters override the behavior specified by \code{print_friendly}.
#'
#' \code{axis_label_size}, \code{legend_title_size}, \code{legend_text_size} and \code{linewidth} are
#' self-explanatory parameters to tune a few aesthetics.
#' Besides, the function returns a ggplot object which can be modified a posteriori using the ggplot
#' interface.
#'
#' @return A \code{ggplot} object representing precision-recall curves.
#' @examples
#'
#' labels = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE) # 8 pieces of data
#'
#' ranking1 = c(1,1,8,3,7,6,4,5)
#' ranking2 = c(4,6,3,3,2,5,6,6)
#' ranking3 = c(6,-1,3,3,2,4,1,5)
#'
#' scores = cbind(ranking1, ranking2, ranking3)
#' colnames(scores) = c("method a", "method b", "method c")
#'
#' # Default plot
#' plot_precall(labels, scores)
#'
#' # Black and white plot
#' plot_precall(labels, scores, print_friendly=TRUE)
#'
#' # Print-friendly background with colored curves
#' plot_precall(labels, scores, print_friendly=TRUE, use_color=TRUE, use_linetype=FALSE)
#'
#' @export
plot_precall <- function(labels, scores, print_friendly = FALSE, use_color, use_linetype,
                         axis_label_size=15, legend_title_size=13, legend_text_size=10,
                         linewidth=0.7)
{

  if(missing(scores)) scores = matrix(seq_along(labels))
  if(missing(use_color)) use_color = !print_friendly
  if(missing(use_linetype)) use_linetype = print_friendly

  if(scores %>% is.vector) scores = matrix(scores)
  check_arguments(labels, scores)

  # Each column corresponds to the ranking of a method.
  meth_names = paste("ranking", seq_len(ncol(scores)))
  col_names = colnames(scores)
  if(!is.null(col_names)){
    meth_names[col_names != ""] = col_names[col_names != ""]
  }

  coord_matrices = plyr::alply(.data=scores, .margins=2,
                               .fun=precall_interpolation,
                               labels=labels)
  names(coord_matrices) = meth_names
  coord_matrices = lapply(meth_names, FUN=function(n) cbind(coord_matrices[[n]],n))
  coord_matrix = do.call(rbind, coord_matrices)
  coord_matrix = data.frame(coord_matrix)
  colnames(coord_matrix) = c("precision", "recall", "Method")

  # Creating the PR-curve
  aesthetics = ggplot2::aes(x=recall, y=precision, color=Method, linetype=Method)
  if(!use_color)    aesthetics$colour   = NULL
  if(!use_linetype) aesthetics$linetype = NULL

  plot = ggplot2::ggplot(coord_matrix, aesthetics)
  plot = plot + ggplot2::geom_line(size=linewidth)

  plot = plot + ggplot2::scale_x_continuous(limits = c(0,1), name="Recall")
  plot = plot + ggplot2::scale_y_continuous(limits = c(0,1), name="Precision")
  if(print_friendly) plot = plot + ggplot2::theme_bw()
  plot = plot + ggplot2::theme(axis.title   = ggplot2::element_text(size=axis_label_size,   face="bold"))
  plot = plot + ggplot2::theme(legend.title = ggplot2::element_text(size=legend_title_size, face="bold"))
  plot = plot + ggplot2::theme(legend.text  = ggplot2::element_text(size=legend_text_size))

  return(plot)
}


#' Computes the point coordinates of a precision-recall curve
#'
#' @param scores A vector of numeric values
#' @param labels A vector of logical values
#' @param nb_interpolation_points A numeric
precall_interpolation <- function(labels, scores, nb_interpolation_points=30)
{
  unique_scores <- sort(unique(scores), decreasing = FALSE)

  if(length(unique_scores)==1) {
    precision = sum(labels)/length(labels)
    precisions = c(precision, precision)
    recalls = c(0,1)
    return(data.frame(precision = precisions, recall = recalls))
  }

  cumsum_num   = sapply(unique_scores, function(s) sum(scores[ labels] <= s))
  cumsum_denom = sapply(unique_scores, function(s) sum(scores <= s))


  interp_num = sapply(seq_len(length(cumsum_num) - 1), function(i) {
    seq(cumsum_num[i], cumsum_num[i + 1], length = nb_interpolation_points)
  })
  interp_denom = sapply(seq_len(length(cumsum_num) - 1), function(i) {
    seq(cumsum_denom[i], cumsum_denom[i + 1], length = nb_interpolation_points)
  })

  dim(interp_num) = NULL
  dim(interp_denom) = NULL

  precisions = interp_num/interp_denom
  recalls = interp_num/sum(labels)
  precisions = c(precisions[1], precisions)
  recalls = c(0, recalls)

  drop = sapply(2:(length(recalls)-1), function(i) recalls[i-1]==recalls[i+1])
  drop = c(FALSE, drop, FALSE)

  recalls = recalls[!drop]
  precisions = precisions[!drop]

  return(data.frame(precision = precisions, recall = recalls))
}


#' Compute the AUPR for precision-recall curves.
#'
#' Compute the Area Under the Precision-Recall curve (AUPR) for one or multiple precision-recall curves.
#'
#' @param scores A matrix or vector of numeric values that specifies the rankings
#' @param labels A vector of logical values that encodes the POSITIVE/NEGATIVE status of data
#'
#' @return The vector of AUPR's.
#'
#' @details
#'
#' The \code{labels} encode the POSITIVE/NEGATIVE (\code{TRUE}/\code{FALSE}) status of data
#' while each column of \code{scores} provides scores corresponding to a ranking of those data:
#' a smaller score means a smaller rank.
#' A vector can be used as \code{scores} instead of a 1-column matrix to compute the AUPR of
#' only one ranking. If no \code{scores} are provided, it is assumed that the labels are already
#' sorted and the function computes the corresponding AUPR.
#'
#' If \code{scores} is a k-column matrix, the function returns a vector of length k, otherwise it
#' returns a scalar.
#'
#' The area is computed from precision-recall curves interpolated using the principle of partial
#' acceptance (see vignette \code{precall} for more details).
#'
#' @examples
#'
#' labels = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE) # 8 pieces of data
#'
#' ranking1 = c(1,1,8,3,7,6,4,5)
#' ranking2 = c(4,6,3,3,2,5,6,6)
#' ranking3 = c(6,-1,3,3,2,4,1,5)
#'
#' scores = cbind(ranking1, ranking2, ranking3)
#' colnames(scores) = c("method a", "method b", "method c")
#'
#' # Implicit ranking
#' aupr(labels)
#' aupr(labels) == aupr(labels, seq_along(labels)) # is TRUE
#'
#' # Explicit single ranking
#' aupr(labels, ranking1)
#'
#' # Explicit multiple rankings
#' aupr(labels, scores)
#'
#' @export
aupr <- function(labels, scores){

  if(missing(scores)) scores = matrix(seq_along(labels))
  if(scores %>% is.vector) scores = matrix(scores)
  check_arguments(labels, scores)

  apply(scores, 2, function(s) {compute_aupr(labels,s)})
}


compute_aupr <- function(labels, scores){

    unique_scores  <- sort(unique(scores), decreasing = FALSE)
    total_positives <- sum(labels)

    true_positives <- sapply(unique_scores, function(s) sum(scores[labels] <= s))
    positives      <- sapply(unique_scores, function(s) sum(scores <= s))


    if(true_positives[1] != 0){
      area = true_positives[1] / total_positives
      area = area * true_positives[1] / positives[1]
    } else {
      area = 0
    }

    len = length(unique_scores) - 1
    for(i in seq_len(len)){

      tp1 = true_positives[i]
      tp2 = true_positives[i+1]

      if(tp1 == tp2) next

      p1 = positives[i]
      p2 = positives[i+1]

      area = area + .aupr_piece(tp1, tp2, p1, p2, total_positives)
    }

    return(area)
}


#' Computes a partial AUPR
#'
#' Computes a partial AUPR (area under precision-recall curve)
#'
#' @param p Total number of positives
#' @param p1 Number of positives at position 1
#' @param p2 Number of positives at position 2
#' @param tp1 Number of true positives at position 1
#' @param tp2 Number of true positives at position 2
#'
#' @return Return the aupr between position 1 and position 2.
.aupr_piece <- function(tp1, tp2, p1, p2, p){
  area <- log(p2/p1) * (tp1 * p2 - tp2 * p1) / (p1 - p2)^2
  area <- area + (tp1 - tp2)/(p1 - p2)
  area <- area * (tp2-tp1)/p

  return(area)
}


# Check the validity of scores and labels arguments.
check_arguments <- function(labels, scores) {
  assertthat::assert_that(scores %>% is.numeric, msg="scores must be a numeric matrix or vector")
  assertthat::assert_that(scores %>% is.matrix,  msg="scores must be a numeric matrix or vector")
  assertthat::assert_that(labels %>% is.logical, msg="labels must be a vector of logical values")
  assertthat::assert_that(labels %>% is.vector,  msg="labels must be a vector of logical values")

  assertthat::assert_that(length(labels) == nrow(scores),
                          msg="the number of rows of scores must be equal to the length of labels.")
}
