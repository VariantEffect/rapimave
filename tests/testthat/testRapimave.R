library(rapimave)

context("MaveDB API access")


test_that("getUser() works", {
	mave <- new.rapimave()

	users <- mave$getAllUsers()
	print(users)

	alan <- mave$getUser("0000-0003-1474-605X")
	print(alan)
	expect_equal("Alan",alan$getFirstName())

})


test_that("getExperimentSet() works", {
	mave <- new.rapimave()

	sets <- mave$getAllExperimentSets()
	print(sets)

	set1 <- mave$getExperimentSet("EXPS000001")
	print(set1)
	expect_equal("EXP000001A",set1$getExperiments())

})

test_that("getExperiment() works", {
	mave <- new.rapimave()

	exps <- mave$getAllExperiments()
	print(exps)

	exp1 <- mave$getExperiment("EXP000001A")
	print(exp1)
	expect_equal("EXPS000001",exp1$getExperimentSet())

})

test_that("getScoreSet() works", {
	mave <- new.rapimave()

	sets <- mave$getAllScoreSets()
	print(sets)

	set <- mave$getScoreSet("SCS000001A.2")
	print(set)
	expect_equal("hgvs",set$getScoreColumns()[[1]])

})

test_that("getScores() works", {
	mave <- new.rapimave()

	scores <- mave$getScores("SCS000001A.2")
	print(head(scores))

	counts <- mave$getCounts("SCS000001A.2")
	print(head(counts))

})
