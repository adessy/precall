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

context("Area under the curve")

test_that("AUPR == 1", {
  scores = seq_len(5)
  labels0 = c(T,T,T,F,F)

  expect_equal(aupr(labels0, scores), 1)

  scores  = c(3, 1, 2, 0)
  labels1 = c(F, F ,F ,T)
  labels2 = c(F, T ,F ,T)
  labels3 = c(F, T ,F ,T)
  labels4 = c(F, T ,T ,T)
  labels5 = c(T, T ,T ,T)

  testthat::expect_equal(aupr(labels1, scores), 1)
  testthat::expect_equal(aupr(labels2, scores), 1)
  testthat::expect_equal(aupr(labels3, scores), 1)
  testthat::expect_equal(aupr(labels4, scores), 1)
  testthat::expect_equal(aupr(labels5, scores), 1)
})

test_that("AUPR == 0", {
  scores1 = c(-1, 5, 2, 6, -3)
  scores2 = c(-4, 3, 0, 6, -1)
  labels  = rep(F,5)

  testthat::expect_equal(aupr(labels, scores1), 0)
  testthat::expect_equal(aupr(labels, scores2), 0)
  testthat::expect_equal(aupr(labels, cbind(scores1,scores2)), c(scores1=0, scores2=0))
  testthat::expect_equal(aupr(labels, cbind(a=scores1,b=scores2)), c(a=0, b=0))
})

test_that("0 < AUPR < 1", {

  scores = c(1,1)
  testthat::expect_equal(aupr(c(T,F), scores), 0.5)
  testthat::expect_equal(aupr(c(F,T), scores), 0.5)

  testthat::expect_equal(aupr(c(T,T,F,F), c(2,1,2,1)), 0.5)
  testthat::expect_equal(aupr(c(F, T), c(0,1)), 1 - log(2))
  testthat::expect_equal(aupr(c(T, F, T), c(0,1,2)), 1 - log(3/2)/2 )
  testthat::expect_equal(aupr(c(T, F, T), c(0,1,1)), 0.75 + log(3)/8 )

  scores = cbind(s1=c(0,1,2), s2=c(0,1,1))
  labels = c(T, F, T)
  aupr_res = c(s1=1 - log(3/2)/2, s2=0.75 + log(3)/8)
  testthat::expect_equal(aupr(labels, scores), aupr_res)
})
