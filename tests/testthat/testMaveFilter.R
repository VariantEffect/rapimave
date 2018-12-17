# Copyright (C) 2018  Jochen Weile, Roth Lab
#
# This file is part of rapimave.
#
# rapimave is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# rapimave is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with rapimave.  If not, see <https://www.gnu.org/licenses/>.

library(rapimave)
library(hgvsParseR)

context("MaveDB data filtering")

pBuilder <- new.hgvs.builder.p(aacode=3)
mockData <- with(pBuilder,data.frame(
	hgvs=c(
		substitution(10,"K","A"),
		cis(substitution(12,"A","D"),substitution(21,"K","E")),
		cis(substitution(6,"A","K"),substitution(14,"S","I"),substitution(14,"W","*")),
		cis(substitution(2,"D","H"),substitution(16,"K","A"))
	),
	score=c(0.1,0.4,0.22,0.85),
	provenance=factor(c("experimental","imputed","imputed","experimental")),
	stringsAsFactors=FALSE
))

cBuilder <- new.hgvs.builder.c()
hgvs.c <- with(cBuilder,c(
		substitution(30,"A","C"),
		cis(substitution(37,"A","T"),substitution(64,"C","G")),
		cis(substitution(18,"G","A"),substitution(43,"A","C"),substitution(43,"C","T")),
		cis(substitution(6,"C","A"),substitution(50,"G","A"))
))
mockData2 <- mockData
mockData2$hgvs <- paste0(hgvs.c," (",mockData2$hgvs,")")

test_that("mutationCount() works", {

	mfilter <- new.mave.filter(mockData)
	result <- which(mfilter$mutationCount(min=2))

	expect_equivalent(result,c(2,3,4))

	mfilter <- new.mave.filter(mockData2)
	result <- which(mfilter$mutationCount(min=2,level="coding"))

	expect_equivalent(result,c(2,3,4))
})

test_that("position() works", {

	mfilter <- new.mave.filter(mockData)
	result <- which(mfilter$position(min=10,max=15))
	expect_equivalent(result,c(1,2,3))

	result <- which(mfilter$position(min=10,max=15,multi="all"))
	expect_equivalent(result,1)
})

test_that("residues() works", {

	mfilter <- new.mave.filter(mockData)
	result <- which(mfilter$residues(to="Ala"))
	expect_equivalent(result,c(1,4))

	result <- which(mfilter$residues(to="Ala",multi="all"))
	expect_equivalent(result,c(1))
})

test_that("numerical() works", {

	mfilter <- new.mave.filter(mockData)
	result <- which(mfilter$numerical("score",min=0.1,max=0.3))
	expect_equivalent(result,c(1,3))
})

test_that("categorical() works", {

	mfilter <- new.mave.filter(mockData)
	result <- which(mfilter$categorical("provenance","imputed"))
	expect_equivalent(result,c(2,3))
})

test_that("combinatorial filtering works", {

	mfilter <- new.mave.filter(mockData)
	result <- which(with(mfilter,
		categorical("provenance","imputed") & mutationCount(max=2)
	))
	expect_equivalent(result,2)

	result <- which(with(mfilter,
		position(min=5,multi="all") & residues(to="Ala") & numerical("score",min=0.1)
	))
	expect_equivalent(result,1)
})
