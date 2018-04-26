library(rapimave)

context("MaveDB API access")

url <- "http://ec2-13-210-169-246.ap-southeast-2.compute.amazonaws.com/api/"

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
	expect_equal("urn:mavedb:00000001-a",set1$getExperiments())

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
	expect_equal("hgvs",set$getScoreColumns()[[1]])

})

test_that("getScores() works", {
	mave <- new.rapimave(baseURL=url)

	scores <- mave$getScores("urn:mavedb:00000001-a-1")
	print(head(scores))

	counts <- mave$getCounts("urn:mavedb:00000001-a-1")
	print(head(counts))

})

test_that("search function works", {
	mave <- new.rapimave(baseURL=url)

	ssets <- mave$findScoreSets("UBE2I")
	print(ssets)
	expect_length(ssets,1)

	ssets <- mave$findScoreSets("foobar")
	print(ssets)
	expect_length(ssets,0)

})
