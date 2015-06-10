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

context("Subroutines to plot PR-curves")

approximate_aupr <- function(labels, scores, nb_interpolation_points) {

  coords = precall_interpolation(labels, scores, nb_interpolation_points)
  precision = coords$precision
  recall = coords$recall

  base =  tail(recall,-1) - head(recall,-1)
  height = (head(precision,-1) + tail(precision,-1))/2
  approx_aupr = sum(base*height)

  return(approx_aupr)
}


test_that("Interpolation can be used to approximate the aupr...", {

  scores1 = c(10, 5, 5, 4, 7, 5, 3, 8, 2, 4)
  scores2 = c(10, 7, 3, 2, 6, 2, 7, 10, 9, 7)
  scores3 = c(2, 6, 8, 6, 1, 3, 5, 3, 6, 8)
  scores4 = c(7, 7, 10, 9, 4, 1, 8, 8, 7, 2)
  scores5 = c(10, 8, 4, 1, 1, 8, 5, 8, 1, 2)

  labels = c(T,T,T,F,F,T,F,T,T,F)

  relative_delta <- function(scores, labels) {
    aupr_approx = approximate_aupr(labels, scores, 1000)
    aupr_analytic = aupr(labels, scores)
    abs(aupr_approx - aupr_analytic) / aupr_analytic
  }

  delta_1 = relative_delta(scores1, labels)
  delta_2 = relative_delta(scores2, labels)
  delta_3 = relative_delta(scores3, labels)
  delta_4 = relative_delta(scores4, labels)
  delta_5 = relative_delta(scores5, labels)

  testthat::expect_less_than(delta_1, 1e-7)
  testthat::expect_less_than(delta_2, 1e-7)
  testthat::expect_less_than(delta_3, 1e-7)
  testthat::expect_less_than(delta_4, 1e-7)
  testthat::expect_less_than(delta_5, 1e-7)
})
