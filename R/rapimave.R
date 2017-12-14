
library(httr)
library(hgvsParseR)
library(RJSONIO)


#' New R-API MaveDB User
#'
#' Internal constructor that creates an immutable new user object. This function is not exported
#'   and should not be called outside of the rapimave package itself. User objects are generated
#'   by the \code{getAllUsers()} and \code{getUser()} methods of a rapimave object (which itself
#'	 is generated using the \code{new.rapimave()} constructor). However, this section explains the
#'   functions that are available for these objects.
#'
#' A user object offers the following getter functions:
#' \itemize{
#'   \item{getUsername()} return the username.
#'   \item{getFirstName()} return the user's first name.
#'   \item{getLastName()} return the user's last name.
#'   \item{getExperimentSets()} return the ExperimentSets attributed to the user.
#'   \item{getExperiments()} return the Experiments attributed to the user.
#' }
#'
#' @return a new R-API MaveDB user object.
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' user <- mave$getUser("0000-0003-1474-605X")
#' cat(user$getFirstName(),user$getLastName())
#' }
new.user <- function(data) {
	.data <- data
	expectedFields <- c("username","first_name","last_name","experimentsets","experiments")
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.user()")
	}
	#TODO: Validate data!
	structure(list(
		getUsername=function() .data$username,
		getFirstName=function() .data$first_name,
		getLastName=function() .data$last_name,
		getExperimentSets=function() .data$experimentsets,
		getExperiments=function() .data$experiments
	),class="rapimaveUser")
}

#' Print R-API MaveDB User
#'
#' Prints a human-readable summary of a R-API MaveDB User object:
#'
#' @param obj the User object to print
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave$getUser("0000-0003-1474-605X"))
#' }
print.rapimaveUser <- function(obj) {
	cat(paste0(
		"MaveDB user ",obj$getUsername(),
		" (",obj$getFirstName()," ",obj$getLastName(),")","\n"
	))
}


#' New R-API MaveDB ExperimentSet
#'
#' Internal constructor that creates an immutable new ExperimentSet object. This function is not exported
#'   and should not be called outside of the rapimave package itself. ExperimentSet objects are generated
#'   by the \code{getAllExperimentSets()} and \code{getExperimentSet()} methods of a rapimave object (which itself
#'	 is generated using the \code{new.rapimave()} constructor). However, this section explains the
#'   functions that are available for these objects.
#'
#' A ExperimentSet object offers the following getter functions:
#' \itemize{
#'   \item{getExperiments()} returns the accessions of the experiments belonging to this ExperimentSet.
#'   \item{getAuthors()} returns the usernames of the authors of this ExperimentSet.
#'   \item{getAccession()} returns the accession of this ExperimentSet.
#' }
#'
#' @return a new R-API MaveDB ExperimentSet object.
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' set1 <- mave$getExperimentSet("EXPS000001")
#' expAccessions <- set1$getExperiments()
#' }
new.experimentSet <- function(data) {
	.data <- data
	expectedFields <- c("experiments","authors","accession")
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.experimentSet()")
	}
	#TODO: Validate data!
	structure(list(
		getExperiments=function() .data$experiments,
		getAuthors=function() .data$authors,
		getAccession=function() .data$accession
	),class="rapimaveExpSet")
}

#' Print R-API MaveDB ExperimentSet
#'
#' Prints a human-readable summary of a R-API MaveDB ExperimentSet object:
#'
#' @param obj the ExperimentSet object to print
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave$getExperimentSet("EXPS000001"))
#' }
print.rapimaveExpSet <- function(obj) {
	cat("MaveDB ExperimentSet",obj$getAccession(),"\n")
}

#' New R-API MaveDB Experiment
#'
#' Internal constructor that creates an immutable new Experiment object. This function is not exported
#'   and should not be called outside of the rapimave package itself. Experiment objects are generated
#'   by the \code{getAllExperiments()} and \code{getExperiment()} methods of a rapimave object (which itself
#'	 is generated using the \code{new.rapimave()} constructor). However, this section explains the
#'   functions that are available for these objects.
#'
#' A Experiment object offers the following getter functions:
#' \itemize{
#'   \item{getScoreSets()} return the list of accessions of the Experiment's ScoreSets.
#'   \item{getAuthors()} returns the usernames of the authors of this Experiment.
#'   \item{getAccession()} return the accession of this Experiment.
#'   \item{getExperimentSet()} return the accession of the ExperimentSet to which this Experiment belongs.
#' }
#'
#' @return a new R-API MaveDB experiment object.
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' exp1 <- mave$getExperiment("EXP000001A")
#' authorIds <- exp1$getAuthors()
#' }
new.experiment <- function(data) {
	.data <- data
	expectedFields <- c("scoresets","authors","accession","experimentset")
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.experiment()")
	}
	#TODO: Validate data!
	structure(list(
		getScoreSets=function() .data$scoresets,
		getAuthors=function() .data$authors,
		getAccession=function() .data$accession,
		getExperimentSet=function() .data$experimentset
	),class="rapimaveExperiment")
}

#' Print R-API MaveDB Experiment
#'
#' Prints a human-readable summary of a R-API MaveDB Experiment object:
#'
#' @param obj the Experiment object to print
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave$getExperiment("EXP000001A"))
#' }
print.rapimaveExperiment <- function(obj) {
	cat("MaveDB Experiment",obj$getAccession(),"\n")
}

#' New R-API MaveDB ScoreSet
#'
#' Internal constructor that creates an immutable new ScoreSet object. This function is not exported
#'   and should not be called outside of the rapimave package itself. ScoreSet objects are generated
#'   by the \code{getAllScoreSets()} and \code{getScoreSet()} methods of a rapimave object (which itself
#'	 is generated using the \code{new.rapimave()} constructor). However, this section explains the
#'   functions that are available for these objects.
#'
#' A ScoreSet object offers the following getter functions:
#' \itemize{
#'   \item{getAuthors()} returns the usernames of the authors of this ScoreSet.
#'   \item{getAccession()} returns the accession of this ScoreSet.
#'   \item{getLicence()} returns the licencing information governing the use of this ScoreSet.
#'   \item{getCurrentVersion()} If a more up-to-date version of this ScoreSet exists, 
#'     that has since replaced it, this function will return the accession of the most recent version.
#'   \item{getReplaces()} If this ScoreSet replaces an older ScoreSet, this function returns
#'     the accession of that older ScoreSet.
#'   \item{getReplacedBy()} If this ScoreSet was replaced by newer ScoreSet, this function returns
#'     the accession of that newer ScoreSet in versioning history. To obtain the most recent version,
#'     use \code{getCurrentVersion()}.
#'   \item{getCountColumns()} returns the column names in the count table for this ScoreSet.
#'   \item{getScoreColumns()} returns the column names in the score table for this ScoreSet.
#' }
#'
#' @return a new R-API MaveDB ScoreSet object.
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' set1 <- mave$getScoreSet("SCS000001A.2")
#' scoreColNames <- set1$getScoreColumns()
#' }
new.scoreSet <- function(data) {
	.data <- data
	expectedFields <- c(
		"authors","accession","licence","current_version",
		"replaces","replaced_by","count_columns","score_columns"
	)
	if (!is.list(data) || !all(expectedFields %in% names(.data))) {
		stop("Illegal argument for new.experimentSet()")
	}
	#TODO: Validate data!
	structure(list(
		getAuthors=function() .data$authors,
		getAccession=function() .data$accession,
		getLicence=function() .data$licence,
		getCurrentVersion=function() .data$current_version,
		getReplaces=function() .data$replaces,
		getReplacedBy=function() .data$replaced_by,
		getCountColumns=function() .data$count_columns,
		getScoreColumns=function() .data$score_columns
	),class="rapimaveScoreSet")
}

#' Print R-API MaveDB ScoreSet
#'
#' Prints a human-readable summary of a R-API MaveDB ScoreSet object:
#'
#' @param obj the ScoreSet object to print
#' @keywords MaveDB ScoreSet
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave$getScoreSet("SCS000001A.2"))
#' }
print.rapimaveScoreSet <- function(obj) {
	cat("MaveDB ScoreSet ",obj$getAccession(),"\n")
}

#' MaveDB R-API client constructor
#'
#' Creates an object of type rapimave.
#'
#' The resulting object offers the following functions:
#' \itemize{
#' 	\item{getAllUsers()} returns a list of all user objects. 
#'     See \code{\link{new.user}} for available methods on user objects.
#' 	\item{getUser(username)} returns the user object for the given username. 
#'     Usernames are usually ORCID IDs. See https://orcid.org/ for information on ORCID.
#'     See \code{\link{new.user}} for available methods on user objects.
#' 	\item{getAllExperimentSets()} returns a list of all ExperimentSet objects.
#'     See \code{\link{new.experimentSet}} for available methods on ExperimentSet objects.
#' 	\item{getExperimentSet(accession)} returns the ExperimentSet object for the given accession.
#'     ExperimentSet accessions usually follow the syntax /EXPS\d+/ .
#'     See \code{\link{new.experimentSet}} for available methods on ExperimentSet objects.
#' 	\item{getAllExperiments()} returns a list of all Experiment objects.
#'     See \code{\link{new.experiment}} for available methods on Experiment objects.
#' 	\item{getExperiment(accession)} returns the Experiment object for the given accession.
#'     Experiment accessions usually follow the syntax /EXP\d+\w[1]/ .
#'     See \code{\link{new.experiment}} for available methods on Experiment objects.
#' 	\item{getAllScoreSets()} returns a list of all ScoreSet objects.
#'     See \code{\link{new.scoreSet}} for available methods on ScoreSet objects.
#' 	\item{getScoreSet(accession)} returns the ScoreSet object for the given accession.
#'     ScoreSet accessions usually follow the syntax /SCS\d+\w[1]\.\d[1]/ .
#'     See \code{\link{new.scoreSet}} for available methods on ScoreSet objects.
#' 	\item{getScores(accession)} returns a \code{data.frame} with the scores for the given ScoreSet accession
#' 	\item{getCounts(accession)} returns a \code{data.frame} with the counts for the given ScoreSet accession
#' }
#'
#' @param baseURL MaveDB API base-URL. Defaults to "https://www.mavedb.org/api/"
#' @param certifySSL Logical indicating whether to certify SSL certificates when querying
#'     the database. Defaults to \code{FALSE}.
#' @param encoding A string specifying the expected character encoding used by the webservice.
#'     Defaults to "UTF-8"
#' @return MaveDB R-API object with functions accessbile via the $ operator.
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' }
new.rapimave <- function(baseURL="https://www.mavedb.org/api/",certifySSL=FALSE,encoding="UTF-8") {

	if (!certifySSL) {
		set_config(config(ssl_verifypeer = 0L))
	}

	getAllUsers <- function() {
		url <- paste0(baseURL,"get/user/all/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0 && "users" %in% names(returnData)) {
				if (length(returnData$users) > 0) {
					return(lapply(returnData$users, new.user))
				} else {
					return(list())
				}
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getUser <- function(username) {
		url <- paste0(baseURL,"get/user/",username,"/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(new.user(returnData))
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such user!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getAllExperimentSets <- function() {
		url <- paste0(baseURL,"get/experimentset/all/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0 && "experimentsets" %in% names(returnData)) {
				if (length(returnData$experimentsets) > 0) {
					return(lapply(returnData$experimentsets, new.experimentSet))
				} else {
					return(list())
				}
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getExperimentSet <- function(accession) {
		url <- paste0(baseURL,"get/experimentset/",accession,"/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(new.experimentSet(returnData))
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such ExperimentSet!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getAllExperiments <- function() {
		url <- paste0(baseURL,"get/experiment/all/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0 && "experiments" %in% names(returnData)) {
				if (length(returnData$experiments) > 0) {
					return(lapply(returnData$experiments, new.experiment))
				} else {
					return(list())
				}
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getExperiment <- function(accession) {
		url <- paste0(baseURL,"get/experiment/",accession,"/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(new.experiment(returnData))
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such Experiment!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getAllScoreSets <- function() {
		url <- paste0(baseURL,"get/scoreset/all/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0 && "scoresets" %in% names(returnData)) {
				if (length(returnData$scoresets) > 0) {
					return(lapply(returnData$scoresets, new.scoreSet))
				} else {
					return(list())
				}
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getScoreSet <- function(accession) {
		url <- paste0(baseURL,"get/scoreset/",accession,"/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- fromJSON(content(htr,as="text",encoding=encoding))
			if (length(returnData) > 0) {
				return(new.scoreSet(returnData))
			} else {
				stop("Unexpected return value: ",returnData)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such ScoreSet!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getScores <- function(accession) {
		url <- paste0(baseURL,"get/scoreset/",accession,"/scores/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- content(htr,as="text",encoding=encoding)
			if (nchar(returnData) > 0) {
				con <- textConnection(returnData,open="r")
				scoreTable <- read.csv(con,stringsAsFactors=FALSE)
				close(con)
				return(scoreTable)
			} else {
				return(NA)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such ScoreSet!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	getCounts <- function(accession) {
		url <- paste0(baseURL,"get/scoreset/",accession,"/counts/")
		htr <- GET(url)
		if (http_status(htr)$category == "Success") {
			returnData <- content(htr,as="text",encoding=encoding)
			if (nchar(returnData) > 0) {
				con <- textConnection(returnData,open="r")
				countTable <- read.csv(con,stringsAsFactors=FALSE)
				close(con)
				return(countTable)
			} else {
				return(NA)
			}
		} else if (http_status(htr)$category == "Not Found") {
			stop("No such ScoreSet!")
		} else {
			stop("MaveDB server message: ",http_status(htr)$message)
		}
	}

	structure(list(
		getAllUsers=getAllUsers,
		getUser=getUser,
		getAllExperimentSets=getAllExperimentSets,
		getExperimentSet=getExperimentSet,
		getAllExperiments=getAllExperiments,
		getExperiment=getExperiment,
		getAllScoreSets=getAllScoreSets,
		getScoreSet=getScoreSet,
		getScores=getScores,
		getCounts=getCounts
	),class="rapimave")
}


#' Print R-API MaveDB object
#'
#' Prints a human-readable summary of a R-API MaveDB object.
#'
#' Prints a list of available functions for the object.
#'
#' @param obj the object to print
#' @keywords MaveDB
#' @export
#' @examples
#' \dontrun{
#' mave <- new.rapimave()
#' print(mave)
#' }
print.rapimave <- function(obj) {
	cat(paste0(
		"A rapimave API object.\n\n",
		"The following functions are available:\n",
		paste0(names(obj),"()",collapse="; ")
	),"\n")
}
