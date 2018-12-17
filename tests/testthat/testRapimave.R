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

context("MaveDB API access")

# url <- "http://ec2-13-210-169-246.ap-southeast-2.compute.amazonaws.com/api/"
url <- "https://www.mavedb.org/api/"

test_that("getUser() works", {
	mave <- new.rapimave(baseURL=url)

	users <- mave$getAllUsers()
	print(users)

	alan <- mave$getUser("0000-0003-1474-605X")
	print(alan)
	expect_equal("Alan",alan$getFirstName())

})


test_that("URN detection works", {
	mave <- new.rapimave(baseURL=url)

	expect_error(mave$getExperimentSet("EXPS000001"))

})

test_that("getExperimentSet() works", {
	mave <- new.rapimave(baseURL=url)

	sets <- mave$getAllExperimentSets()
	print(sets)

	set1 <- mave$getExperimentSet("urn:mavedb:00000001")
	print(set1)
	expect_equal("urn:mavedb:00000001-a",set1$getExperiments()[[1]])

})

test_that("getExperiment() works", {
	mave <- new.rapimave(baseURL=url)

	exps <- mave$getAllExperiments()
	print(exps)

	exp1 <- mave$getExperiment("urn:mavedb:00000001-a")
	print(exp1)
	expect_equal("urn:mavedb:00000001",exp1$getExperimentSet())

})

test_that("getScoreSet() works", {
	mave <- new.rapimave(baseURL=url)

	sets <- mave$getAllScoreSets()
	print(sets)

	set <- mave$getScoreSet("urn:mavedb:00000001-a-1")
	print(set)
	expect_equal("hgvs_nt",set$getScoreColumns()[[1]])

})

test_that("getScores() works", {
	mave <- new.rapimave(baseURL=url)

	scores <- mave$getScores("urn:mavedb:00000001-a-1")
	expect_gt(nrow(scores),0)
	print(head(scores))

	counts <- mave$getCounts("urn:mavedb:00000001-a-1")
	print(head(counts))

})

test_that("search function works", {
	mave <- new.rapimave(baseURL=url)

	ssets <- mave$findScoreSets("UBE2I")
	print(ssets)
	expect_gt(length(ssets),0)

	ssets <- mave$findScoreSets("foobar")
	print(ssets)
	expect_length(ssets,0)

})


test_that("target data access works", {
	mave <- new.rapimave(baseURL=url)

	set <- mave$getScoreSet("urn:mavedb:00000001-a-1")
	target <- set$getTarget()

	expect_length(target$getSequence(),1)
	
	xref <- target$getXrefUniprot()
	expect_equivalent(xref$getID(),"P63279")

})
