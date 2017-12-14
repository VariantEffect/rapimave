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

	users <- mave$getAllExperimentSets()
	print(users)

	set1 <- mave$getExperimentSet("EXPS000001")
	print(set1)
	expect_equal("EXP000001A",set1$getExperiments())

})
